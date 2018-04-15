library(readxl)
dataset <- read_excel(NULL)
View(dataset)
library(readxl)
install.packages("dplyr")
library(dplyr)
library(readxl)
dir <- "C:\Minsait"
dir <- "C:/Minsait"
filename <- "WiDS data dictionary v2.xlsx"
url <- paste0(dir, filename)
dictionary <- read_excel(url)
dir <- "C:/Minsait/"
filename <- "WiDS data dictionary v2.xlsx"
url <- paste0(dir, filename)
dictionary <- read_excel(url)
View(dictionary)
head(dictionary)
str(dictionary)
class(dictionary)
?tbl_df
# Training file & insights
dir <- "C:/Minsait/"
filename <- "train.xlsx"
url <- paste0(dir, filename)
dictionary <- read_excel(url)
dictionary
trin <- read_excel(url)
train <- read_excel(url)
View(train)
# Training file & insights
dir <- "C:/Minsait/"
filename <- "train.xlsx"
url <- paste0(dir, filename)
train <- read_excel(url)
train <- read.csv(url)
# Training file & insights
dir <- "C:/Minsait/"
filename <- "train.csv"
url <- paste0(dir, filename)
train <- read.csv(url)
View(train)
train
View(train)
class(train)
head(train)
dim(train)
# Test file where to send predicted data
dir <- "C:/Minsait/"
filename <- "test.csv"
url <- paste0(dir, filename)
test <- read.csv(url)
View(test)
head(test, 1)
install.packages("devtools")
devtools::install_github("rstudio/packrat")
packrat::init()
# Sample submission format on how to send testing data.
dir <- "C:/Minsait/"
filename <- "sample_submission.csv"
url <- paste0(dir, filename)
sample_submission <- read.csv(url)
View(sample_submission)
dir <- "C:/Minsait/"
dir <- "C:/Minsait/"
