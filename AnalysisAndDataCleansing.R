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
    geom_histogram(stat="count") + 
    scale_y_continuous(name="Count",labels = scales::comma,limits = c(0,10000)) +
    xlab("is_female (var)") +
    ggtitle(" Female(1) vs Male(1)")

    
