library(SuperLearner)
library(dplyr)
library(caret)
library(e1071)
library(bnlearn)
library(arules)

review_metaweights <- function(cv_sl) {
  meta_weights <- coef(cv_sl)
  means = colMeans(meta_weights)
  sds = apply(meta_weights, MARGIN = 2,  FUN = sd)
  mins = apply(meta_weights, MARGIN = 2, FUN = min)
  maxs = apply(meta_weights, MARGIN = 2, FUN = max)
  sl_stats = cbind("mean(weight)" = means, "sd" = sds, "min" = mins, "max" = maxs)
  sl_stats[order(sl_stats[, 1], decreasing = TRUE), ]
}

detectFactor <- function(x) {
  which(apply(x, 2, function(x) {length(unique(x))}) <= 10)
}

#Attempt to discretize a dataframe with 2 breaks
discretize.DF <- function(x) {
  df <- sapply(x[-detectFactor(x)], as.numeric)
  n = length(colnames(df))
  for (i in 1:n)
    df[, i] <- discretize(df[, i], breaks = 2)
  df
}

#Custom KSVM
SL.ksvm.ANOVA <- function(...) {
  SL.ksvm(..., kernel = "anovadot")
}

#Custom Knn using manhattan distance for the kernel function
SL.kernelKnn.manhattan <- function(...) {
  SL.kernelKnn(..., method = "manhattan", k=10)
}

#Custom Knn using bray-curtis dissimilarity for the kernel function
SL.kernelKnn.braycurtis <- function(...) {
  SL.kernelKnn(..., method = "braycurtis", k=10)
}

#Custom predbagg
SL.ipredbagg.custom <- function(...) {
  SL.ipredbagg(..., nbagg = 150)
}

#Custom Boost
SL.xgboost.custom <- function(...) {
  SL.xgboost(..., max_depth = 8, ntrees = 1200)
}

#Custom Boost
SL.xgboost.custom2 <- function(...) {
  SL.xgboost(..., max_depth = 8, ntrees = 1200, minobspernode = 5)
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

#--------
#data.discrete <- discretize.DF(data)
#--------

#t <- SL.naivebayes(y_train, x_train, x_test, binomial())


#List of the algorithms used to in the ensemble model. We will be using the SuperLearner package to create this model
#Ranger is an implemtation of the Random Forest algorithm
#ipredbagg is a bagging algoritm
#xgboost is a boosting algorithm
#Bayesglm is a linear regression algorithm
#rpartPrune is a decision tree algorithm that used pruning

#lists of algorithms to test out parameter tunings, kernels, and other stuff
#each list will only test one familty of algorithms

ksvmAlgorithms = list("SL.ksvm.ANOVA")
kernelKnnAlgorithms= list("SL.kernelKnn", "SL.kernelKnn.manhattan", "SL.kernelKnn.braycurtis")
rangerAlgorithms = list("SL.ranger")
ipredAlgorithms = list("SL.ipredbagg", "SL.ipredbagg.custom")
xgboostAlgorithms = list("SL.xgboost", "SL.xgboost.custom", "SL.xgboost.custom2")
bayesAlgorithms = list("SL.bayesglm")
rpartAlgorithms = list("SL.rpartPrune")

#List of each algorithm to test
master_algorithm_list = c(ksvmAlgorithms, kernelKnnAlgorithms, rangerAlgorithms, ipredAlgorithms, xgboostAlgorithms, bayesAlgorithms, rpartAlgorithms)

#Cross validation controller parameters, ste too 10 for final result, keep at 2 for speed
num_folds = 2

#This is used to evaluate the performance of the models trained, this does not actually create a model
#This takes a long time to compute, if you just need to predict then dont execute this
cv.model <- CV.SuperLearner(y_train,
                      x_train,
                      V = num_folds,
                      family = binomial(),
                      SL.library = xgboostAlgorithms)

#Final Model Fit
#Stacking, use a binomial instead of guassian because we're not using regression because the number of unique values of the
#decision attribute very low.
# model <- SuperLearner(y_train,
#                       x_train,
#                       family = binomial(),
#                       SL.library = kernelKnnAlgorithms)
# 
# predictions <- predict.SuperLearner(model, newdata = x_test)
# y_result <- as.numeric(ifelse(predictions$pred>=0.5,1,0))
# 
# conf_mat <- confusionMatrix(as.factor(y_test), as.factor(y_result))

