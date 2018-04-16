# Applied libraries for Modeling and Training
library(data.table)
library(stringr)
library(readxl)
library(e1071)
library(rpart)
library(caret)
library(randomForest) # apart from e1071 content ::randoforest
library(xgboost)
library(mboost)
library(pROC)
library(knitr)
library(tidyverse) # conflicts unmasked when library loaded

str(train) # 18,255 obs * 580 vars ==> ready for training
str(test) # 27,285 obs * 1,234 vars
# now we are going to use a classifier ==> 0's and 1's
# 1st approach ==> boosting
# 2nd approach ==> randomForest::reducing variance and for accuracy (while much 
                  # power calculations for parallel needed ==> a Spark approach would be perfect)
# 3rd approach ==> hybrid of boosting Random Forest (time to play)

#-------------------------------------RANDOM FOREST-------------------------------------------------#
randomForest::randomForest(train) #error with NA's
# location ==> depending if the user based on is_female = 1:
if(train$is_female == 1) train$train_id
head(train[, c(1:10)], 20)
train$AA5_town <- ifelse(is.na(train$AA5), 0, 1)
test$AA5_town <- ifelse(is.na(test$AA5), 0, 1)

# working ==> yes / no
train$DL1household <- ifelse(train$DL1==7 ,1,0)
train$DL1student <- ifelse(train$DL1==8 ,1,0)
test$DL1household <- ifelse(test$DL1==7 ,1,0)
test$DL1student <- ifelse(test$DL1==8 ,1,0)

# financial dependency
train$FL4finDep<- ifelse(train$FL4==1,1,0)
test$FL4dinDep<- ifelse(test$FL4==1,1,0)

# Create a set of training and valid sets that will be used in the XGBboost package, keeping the target vaiable as numeric
inTrain <- createDataPartition(y = train$is_female, p = 0.8, list = F)
training <- train[inTrain,]
valid <- train[-inTrain,]
# Set the target variable to be a character factor with levels one and two for use in the training in the caret XGBoost
train$is_female <- as.factor(train$is_female)
levels(train$is_female)[levels(train$is_female)=="0"] <- "one"
levels(train$is_female)[levels(train$is_female)=="1"] <- "two"
# Create a set of training and valid sets that will be used in the caret package, with the target vaiable as factor
inTrain <- createDataPartition(y = train$is_female, p = 0.7, list = F)
training2 <- train[inTrain,]
valid2 <- train[-inTrain,]

# is_female numeric outcome y (label) on training set
y = training$is_female
# To use advanced features xgboost, as recommended, we'll use xgb.DMatrix function to convert a matrix or a dgCMatrix into a xgb.DMatrix object, which contains a list with dgCMatrix data  and numeric label: 
dtrain <- xgb.DMatrix(data = data.matrix(training[ ,-training$is_female]),
                      label = y)
dvalid <- xgb.DMatrix(data = data.matrix(valid[ ,-valid$is_female]), 
                      label = valid$is_female)
dtest <- xgb.DMatrix(data.matrix(test))
# We use watchlist parameter to measure the progress with a second dataset which is already classified. 
watchlist <- list(train=dtrain, test=dvalid)
# Check that dtest has the same number of rows as the original test file, 27285 rows
nrow(dtest)

#----------------------------CROSS VALIDATION-------------------------------------#
set.seed(1234)
# Using booster gbtree, with a large nround=50 
xgbcv <- xgb.cv(params = list(
  # booster = "gbtree", 
  objective = 'binary:logistic'),
  metrics = list("rmse","auc"),
  label=y,
  data = dtrain,
  nrounds = 50,
  nfold = 5, # 5 fold CV
  showsd = T, 
  print_every_n = 10, # when verbose =0
  # early_stopping_rounds = 5, 
  verbose=1,
  prediction = T)
#  nround best iteration is, based on the min test rmse:
it <-  which.min(xgbcv$evaluation_log$test_rmse_mean)
bestiteration <-  xgbcv$evaluation_log$iter[it]
bestiteration

# Plot the RMSE from the CV
xgbcv$evaluation_log %>%
  dplyr::select(iter,train_rmse_mean,test_rmse_mean) %>%
  gather(TestOrTrain, RMSE, -iter) %>%
  ggplot(aes(x = iter, y = RMSE, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()

# Calculate AUC for the xgbcv
xgbcv.ROC <- roc(response = y,
                 predictor = xgbcv$pred)
# Area under the curve: 1?
xgbcv.ROC$auc

