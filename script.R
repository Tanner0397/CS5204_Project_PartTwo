library(SuperLearner)
library(dplyr)
library(caret)
library(e1071)

SL.ranger.tune <- function(...){
  SL.ranger(..., num.trees=1000, mtry=2)
}

data <- read.table("hw8_data.csv", sep = ',', header = TRUE)

#Sample the data randomly to create a testing set and the traning set
sample_size = floor(0.66*nrow(data))
train_index = sample(seq_len(nrow(data)), size = sample_size)
data_train <- data[train_index, ]
data_test <- data[-train_index, ]

y_train <- data_train[, 1]
y_test <- data_test[, 1]

x_train <- data_train[, 2:38]
x_test <- data_test[, 2:38]



#Stacking, use a binomial instead of guassian because we're not using regression because the number of unique values of the
#decision attribute very low
model <- SuperLearner(y_train,
                      x_train,
                      family = binomial(),
                      SL.library = list("SL.ksvm",
                                        "SL.ranger"))
#

predictions <- predict.SuperLearner(model, newdata = x_test)
y_result <- as.numeric(ifelse(predictions$pred>=0.5,1,0))

# cv.model <- CV.SuperLearner(y,
#                             x,
#                             V=5,
#                             family = binomial(), 
#                             SL.library = list("SL.randomForest"))