library(SuperLearner)
library(dplyr)
library(caret)
library(e1071)

review_metaweights <- function(cv_sl) {
  meta_weights <- coef(cv_sl)
  means = colMeans(meta_weights)
  sds = apply(meta_weights, MARGIN = 2,  FUN = sd)
  mins = apply(meta_weights, MARGIN = 2, FUN = min)
  maxs = apply(meta_weights, MARGIN = 2, FUN = max)
  sl_stats = cbind("mean(weight)" = means, "sd" = sds, "min" = mins, "max" = maxs)
  sl_stats[order(sl_stats[, 1], decreasing = TRUE), ]
}

#Custom function for caret that's not verbose
SL.caret1 <- function (Y, X, newX, family, obsWeights, method = "rf", tuneLength = 3, 
                       trControl = trainControl(method = "cv", number = 20, verboseIter = FALSE), 
                       metric,...) 
{
  if (length(unique(Y))>2){
    if(is.matrix(Y)) Y <- as.numeric(Y)
    metric <- "RMSE"
    if(method=="gbm"){
      suppressWarnings(
        fit.train <- caret::train(x = X, y = Y, weights = obsWeights, 
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl,verbose=FALSE)
      )
    }else{
      suppressWarnings(
        fit.train <- caret::train(x = X, y = Y, weights = obsWeights, 
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl)
      )
    }
    pred <- predict(fit.train, newdata = newX, type = "raw")
  }
  if (length(unique(Y))<=2) {
    metric <- "Accuracy"
    Y.f <- as.factor(Y)
    levels(Y.f) <- c("A0", "A1")
    if(method=="gbm"){
      suppressWarnings(
        fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights,
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl, verbose = FALSE)
      )
    }else{
      suppressWarnings(
        fit.train <- caret::train(x = X, y = Y, weights = obsWeights, 
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl)
      )
    }
    pred <- predict(fit.train, newdata = newX, type = "prob")[,2]
  }
  fit <- list(object = fit.train)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.caret")
  return(out)
}

#Custom boosting algorithm that uses caret, and used 5 Fold validation
SL.gbm.custom <- function(...,method="gbm",tuneLength=3, trControl=trainControl(method="cv",number=5,verboseIter=FALSE)){
  SL.caret1(...,method=method,tuneLength=tuneLength,trControl=trControl)
}

#Naive Bayes Network model
SL.naivebayes <- function(Y, X) {
  
}

data <- read.table("hw8_data.csv", sep = ',', header = TRUE)

#Sample the data randomly to create a testing set and the traning set
#Set the seed to the sets are consistant
set.seed(100)
sample_size = floor(0.66*nrow(data))
train_index = sample(seq_len(nrow(data)), size = sample_size)
data_train <- data[train_index, ]
data_test <- data[-train_index, ]

y_train <- data_train[, 1]
y_test <- data_test[, 1]

x_train <- data_train[, 2:38]
x_test <- data_test[, 2:38]


#List of the algorithms used to in the ensemble model. We will be using the SuperLearner package to create this model
#Ranger is an implemtation of the Random Forest algorithm
algorithmList = list("SL.ksvm", "SL.kernelKnn", "SL.ranger", "SL.ipredbagg")

#Cross validation controller parameters
num_folds = 5

#This is used to evaluate the performance of the models trained, this does not actually create a model
cv.model <- CV.SuperLearner(y_train,
                      x_train,
                      V = num_folds,
                      family = binomial(),
                      SL.library = algorithmList)

#Final Model Fit
#Stacking, use a binomial instead of guassian because we're not using regression because the number of unique values of the
#decision attribute very low.
model <- SuperLearner(y_train,
                      x_train,
                      family = binomial(),
                      SL.library = algorithmList)

predictions <- predict.SuperLearner(model, newdata = x_test)
y_result <- as.numeric(ifelse(predictions$pred>=0.5,1,0))

conf_mat <- confusionMatrix(as.factor(y_test), as.factor(y_result))

# cv.model <- CV.SuperLearner(y,
#                             x,
#                             V=5,
#                             family = binomial(), 
#                             SL.library = list("SL.randomForest"))