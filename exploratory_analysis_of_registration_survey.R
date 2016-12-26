################################################################################
# R script that explores any pattern in the data collected during 
# registration survey from registrants of 
# "Introduction to DataScience" workshop held on 2016-12-25.
################################################################################


# Load required libraries
library(magrittr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)


# Folder where data was saved.
data_saved_folder <- "./data"

# Load the data.
survey_data <- read.csv(sprintf("%s/registration_survey_subset.csv", data_saved_folder), 
                        header = TRUE, stringsAsFactors = FALSE)

# Show table headings.
colnames(survey_data)

# Show dimension of data (number of rows, number of columns).
dim(survey_data)

# Details about structure of variable
str(survey_data)


################################################################################
# Column 1: Time stamp
# Date/Time of registration form submission.
################################################################################

# Check the data type of column "Timestamp"
class(survey_data$Timestamp)

# Show the first Timestamp
survey_data$Timestamp[1]

# Try converting the first observation
ymd_hms(strtrim(survey_data$Timestamp[1], 22), tz = "Asia/Kathmandu")

# Create a new column with POSIZct data type.
survey_data$time_stamp <- ymd_hms(strtrim(survey_data$Timestamp, 22), tz = "Asia/Kathmandu")
str(survey_data$time_stamp)

# Plot each registration data as a point on a timeline.
ggplot(survey_data) + 
  geom_point(mapping = aes(x = time_stamp, y = 1))

# Most of the points on the plots above obscures each other
# Lets spread them out a little.
ggplot(survey_data, 
       mapping = aes(x = time_stamp, y = 1)) + 
  geom_point(position = position_jitter(height = 0.01)) + 
  ylim(0.9, 1.1)

# Better to count each points that falls in certain range of time.
ggplot(survey_data) + 
  geom_histogram(mapping = aes(x = time_stamp))

# First and last registration.
survey_data$time_stamp %>% range()

# First and last day of registration.
survey_data$time_stamp %>% range() %>% trunc("days")

# Create a new column with date of registration, 
# truncating time information.
survey_data$date_registered <- survey_data$time_stamp %>% trunc("days") %>% as.POSIXct()

# Count number of observations for each date of registration.
table(as.Date(survey_data$date_registered))

# Plot the counts.
ggplot(survey_data) + 
  geom_histogram(mapping = aes(x = date_registered), 
                 stat = "count")


################################################################################
# Column 2: Name
# Name of registrant.
################################################################################

# Extract first name.
# First name is the first part of name separated by a space.
# Apply a function to each element of column 
# that extracts first name.
survey_data$fn <- sapply(X = survey_data$Name, 
                         FUN = function(x) {
                           strsplit(x, split = " ")[[1]][1] %>% 
                             str_trim()
                         })


# Extract last letter of the first name.
survey_data$fn_ll <- sapply(X = survey_data$fn, 
                            FUN = function(x) {
                              x %>% 
                                str_sub(start = -1, end = -1) %>% 
                                tolower()
                            })

# Usually, female first names ends in a vowel.
# Caution: This rule is NOT always true.
vowels <- c("a", "e", "i", "o", "u")


# Initialize a new column with default value.
survey_data$gender <- "Male"
# Find index of registrants whose 
# last letter of first name is a vowel.
# Overwrite with correct values.
survey_data$gender[which(survey_data$fn_ll %in% vowels)] <- "Female"

# Check whether gender column is properly populated
survey_data$gender
survey_data$gender == "Male"
# Indices of participants with gender inferred from first name
which(survey_data$gender == "Male")
which(survey_data$gender == "Female")

# Review if first names and gender matches
survey_data$fn[which(survey_data$gender == "Male")]
survey_data$fn[which(survey_data$gender == "Female")]

# Correct gender information
# for which manually reivew process found a mistake
survey_data$gender[which(tolower(survey_data$fn) %in% c("krishna", "yankee", "dhruba", "siddhartha"))] <- "Male"

# Count number of participants by gender and date of registration
table(as.Date(survey_data$time_stamp), survey_data$gender)

# Perform statistical significance test on whether 
# registration date and gender has any correlation
survey_data %>% 
  count(date = as.Date(time_stamp), gender) %>% 
  rename(count = n) %>% 
  tidyr::spread(gender, count, fill = 0) %>% 
  as.data.frame() %>% 
  (function(x) {
    row.names(x) <- x$date
    return(x)
  }) %>% 
  select(Female, Male) %>% 
  chisq.test()
# Since p-value is higher than 0.05, 
# we conclude that gender and registration date are not related.


# Create a plot of registrant count per day 
# and color code male and female portion of the counts.
ggplot(survey_data) + 
  geom_histogram(mapping = aes(x = date_registered, 
                               fill = gender), 
                 stat = "count")


# Separate out the plot with gender information in its own panel.
ggplot(survey_data) + 
  geom_histogram(mapping = aes(x = date_registered, 
                               fill = gender), 
                 stat = "count") + 
  facet_wrap(~gender)



################################################################################
# Column 3: College
# The registrant's College of attendance.
################################################################################

survey_data$College %>% 
  table()

survey_data$College %>% 
  unique()

survey_data$College %>% 
  unique() %>% 
  length()

survey_data$College %>% 
  tolower() %>% 
  unique()

survey_data$College %>% 
  tolower() %>% 
  unique() %>% 
  length()

colleges <- survey_data$College %>% 
  tolower() %>% 
  unique() %>% 
  sort()

colleges

# Readers are encouraged to convert college names into consistent form, 
# and analyze if there is a statistical significant relationsip between 
# any two pairs of columns: college, date of registration, gender, etc


################################################################################
# Column 4: semester
# Registrant's semester at the college.
################################################################################

survey_data$Semester

survey_data$Semester %>% 
  unique()

survey_data$Semester %>% 
  unique() %>% 
  length()


## Convert semester information into consistent form


survey_data$Semester <- sapply(survey_data$Semester, 
                               FUN = function(x) tolower(strsplit(x, split = " ")[[1]][1]))

survey_data$Semester %>% 
  unique() %>% 
  length()

  
table(survey_data$Semester)


# Semester info is entered in many forms: text, numeric, rank, etc
# Convert it to consistent form.

sem_as_rank <- str_match(string = survey_data$Semester, pattern = "\\d")

sem_as_rank
sem_as_rank[!is.na(sem_as_rank)]
survey_data$Semester[!is.na(sem_as_rank)]

# Create a mapping table 
# that maps integer and cardinal form of semester info into consistent form
sem_rank_int <- data.frame(semester_char = survey_data$Semester[!is.na(sem_as_rank)], 
                           semester_int = as.integer(sem_as_rank[!is.na(sem_as_rank)]))

# Keep unique rows / Remove duplicated rows
sem_rank_int <- sem_rank_int[!duplicated(sem_rank_int), ]


# all letter form
survey_data$Semester[is.na(sem_as_rank)]

semester_char <- sapply(survey_data$Semester[is.na(sem_as_rank)], 
       FUN = function(x) strsplit(x, split = " ")[[1]][1]) %>% 
  tolower() %>% 
  unique() %>% 
  sort()

semester_char

sem_int_as_char <- sem_as_rank %>% 
  unique() %>% 
  sort()

# Create a mapping table 
# that maps all form of semester indication into consistent form
semester_map <- rbind(sem_rank_int, 
                      data.frame(semester_char = semester_char, 
                                 semester_int = c(8,1,4,NA,3)),
                      data.frame(semester_char = sem_int_as_char, 
                                 semester_int = as.integer(sem_int_as_char)))

survey_data <- merge(x = survey_data %>% 
                       transform(Semester = tolower(Semester)), by.x = "Semester", 
                     y = semester_map, by.y = "semester_char", 
                     all.x = TRUE, sort = FALSE) %>% 
  arrange(time_stamp)

# Count registrant per semester
table(survey_data$semester_int)

# Perform Chi-Squared Test 
# to check whether there was any 
# statistical signifiant correlation between 
# students of certain semester signing up at certain dates
survey_data %>% 
  count(date = as.Date(time_stamp), semester_int) %>% 
  rename(count = n) %>% 
  tidyr::spread(semester_int, count, fill = 0) %>% 
  as.data.frame() %>% 
  (function(x) {
    row.names(x) <- x$date
    return(x)
  }) %>% 
  select(-date) %>% 
  chisq.test()
# Since the reported p-value is less than 0.05,
# we conclude that there was a statistical significance 
# in students from certain semester registers at certain dates


# Plot counts of students for each semester, 
# groupped by registered date
ggplot(survey_data) + 
  geom_histogram(mapping = aes(x = semester_int), 
                 stat = "count") + 
  facet_grid(~date_registered)

# A different view of the plot:
# Plot counts of students registration dates, 
# groupped by semester standing

ggplot(survey_data) + 
  geom_histogram(mapping = aes(x = date_registered), 
                 stat = "count") + 
  facet_grid(~semester_int) + 
  theme(axis.text.x = element_text(angle=90, hjust=1))
  


# Any relation between gender and semester?
ggplot(survey_data) + 
  geom_histogram(mapping = aes(x = semester_int, 
                               fill = gender), 
                 stat = "count") + 
  facet_wrap(~gender)

survey_data %>% 
  count(gender, semester_int) %>% 
  rename(count = n) %>% 
  tidyr::spread(semester_int, count, fill = 0) %>% 
  as.data.frame() %>% 
  select(-gender) %>% 
  chisq.test()



################################################################################
# Columns "5: have you participated before?" and 
# "6: why are you participating this event?" 
# are not part of the dataset that is provided 
# to the participants.
# The folowing code can be adapted to analyse 
# any text vector to generate word cloud.
################################################################################


# Assign concise column names
colnames(survey_data)[5]
colnames(survey_data)[5] <- "participated_before"

colnames(survey_data)[6]
colnames(survey_data)[6] <- "why_participate"

colnames(survey_data)


# Adapted from: https://www.r-bloggers.com/building-wordclouds-in-r/

library(tm)
library(SnowballC)
library(wordcloud)

# A text vector we want to generate wordcloud for
text_vector <- survey_data$why_participate

# Create a data object that is understood by the "tm" package
corpus <- Corpus(VectorSource(text_vector))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, removeWords, c('the', 'this', stopwords('english')))

# Perform word stemming
corpus <- tm_map(corpus, stemDocument)

# Generate word cloud plot
wordcloud(corpus, max.words = 100, random.order = FALSE)


### END ###
