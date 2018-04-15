# Here are first approaches

install.packages("dplyr")
library(dplyr)
library(readxl)

# Dictionary with variables
dir <- "C:/Minsait/"
filename <- "WiDS data dictionary v2.xlsx"
url <- paste0(dir, filename)
dictionary <- read_excel(url)
View(dictionary)
# Training file & insights
dir <- "C:/Minsait/"
filename <- "train.csv"
url <- paste0(dir, filename)
train <- read.csv(url)
View(train)
train
# Test file where to send predicted data
dir <- "C:/Minsait/"
filename <- "test.csv"
url <- paste0(dir, filename)
test <- read.csv(url)
View(test)
# Sample submission format on how to send testing data.
dir <- "C:/Minsait/"
filename <- "sample_submission.csv"
url <- paste0(dir, filename)
sample_submission <- read.csv(url) 
View(sample_submission)









