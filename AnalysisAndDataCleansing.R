# Applied libraries for analysis and data cleansing
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(janitor)
library(data.table)
library(knitr)
library(tidyverse)
library(rmarkdown)

summary(train)
str(train) # 18,255 rows * 1,235 cols
head(train[,c(1:20)])# We must predict is_female variable, located at 10th column
sum_is_female <- sum(train$is_female)
    num_females <- sum_is_female / 18255
    num_females # ~ 54% o/ sample
    train %>% ggplot(aes(as.factor(is_female),fill=as.factor(is_female)),guide=FALSE) + 
    geom_histogram(stat="count") + scale_y_continuous(name="Count",labels = scales::comma,limits = c(0,10000)) +
    xlab("is_female (var)") + ggtitle(" Female(1) vs Male(1)")
class(train)

# Data Cleansing
sum_chr <- sum(is.character(train))
sum_NA <- sum(is.na(train))    
sum_DATA <- sum(!is.na((train)))
isfactors <- train %>% select_if(is.factor)
sum(sapply(train, is.factor))
head(isfactors)
isfactorsBlankSpaces <- sapply(isfactors,function(x) table(x =="")["TRUE"])
isfactorsBlankSpaces[isfactorsBlankSpaces < 10000] # max type on data.frame
tr1 <- isfactors %>% select(-LN2_RIndLngBEOth, -LN2_WIndLngBEOth)
train_without_chars <- train %>% dplyr::select(-one_of(as.character(tr1)))
class(tr1)
sum(is.factor(train_without_chars))
is.character(train_without_chars)
str(train_without_chars)
train_nums <- train %>% select_if(is.numeric)
head(train_nums[, c(1:20)]) # relevant chars cleaned but NAs pendent
sum(is.na(train_nums))
train_nums_without_nas <- map(train_nums, ~sum(is.na(.)))
class(train_nums_without_nas)
sum(is.na(train_nums_without_nas)) # OK ==> NAs removed, ==> class == list
train <- train %>% select(-one_of(names(train_nums_without_nas[train_nums_without_nas>16000])))
class(train)

# Now we are going to proceed to MAP and REDUCE to set valuable variables for modeling:
mybinaryFunction <- function(x){ifelse(length(unique(x))>2,FALSE,TRUE)}
  mybinaryFunction(train$is_female) # OK, TRUE
  mybinaryFunction(train$train_id) # OK, FALSE
binaries <- map(train_nums, ~mybinaryFunction(.))
  class(binaries)
sum_binaries <- sum(binaries == TRUE)
is_log <- train %>% select_if(is.logical)
sum(is.na(is_log))-dim(is_log)[1]*dim(is_log)[2]
train <- train %>% select(-one_of(names(is_log)))
str(train) # Done

#-----------------------------#
length(setdiff(dictionary$"Column Name", names(train))) # cols difference between train and dictionary
# ussing summary per certain cols, we would have all relevant statistical data we could need

