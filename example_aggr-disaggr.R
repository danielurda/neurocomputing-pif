rm(list=ls())
library(caret)

source("common.R")
source("aggr_disaggr.R")


# Modify the size of the autoregressive window 
# Possible values: 7, 14, 21, 28, ... ---> weekly patterns, multiple of 7 days
WINDOW = 14


if (WINDOW == 0 || WINDOW %% 7 != 0) {
     stop("The WINDOW parameter should be a multiple of 7 days, e.g. 7, 14, 21, 28 ...")
}

# load the raw original data
original.data             = read.csv("library_users_2004.csv",  header=T, sep=";", stringsAsFactors=F)
# build a matrix with the aggregated information per week
aggregated.weekly.info    = aggregate.weekly.info(X = original.data)
# transform the aggregated time series into a design matrix that can be analysed through machine learning models
data                      = aggregated.timeseries.to.matrix(X = aggregated.weekly.info, W = WINDOW)
WeekNumber.ToPredict      = data$WeekNumber.ToPredict
data$WeekNumber.ToPredict = NULL


# create 10-folds of equal sizes assigning complete weeks to the folds
set.seed(1234)
cvfolds = data.frame(WeekNumber=unique(WeekNumber.ToPredict),
                     Fold=createFolds(y = seq_len(length(unique(WeekNumber.ToPredict))), k = 10, list = F))


# run 10-fold cross-validation
performance.results = NULL
for (iter in 1:10) {
     idx.train  = which(WeekNumber.ToPredict %in% cvfolds$WeekNumber[cvfolds$Fold != iter])
     idx.test   = which(WeekNumber.ToPredict %in% cvfolds$WeekNumber[cvfolds$Fold == iter])
     data.train = data[idx.train,]
     data.test  = data[idx.test,]

     
     # fit a linear regression model to the training set
     linreg.fit = lm(formula = Yt~., data = data.train)
     # get the aggregated predictions made by the trained model on both the training and testing sets
     train.predictions = predict(object = linreg.fit, newdata = data.train)
     test.predictions  = predict(object = linreg.fit, newdata = data.test)
     
     
     # disaggregate the original and predicted outcome
     train.y    = disaggregate(ADI = aggregated.weekly.info, MaxTrainingWeek = max(WeekNumber.ToPredict[idx.train]),
                               y = data.train$Yt, weeks = WeekNumber.ToPredict[idx.train], type = "truth")
     test.y     = disaggregate(ADI = aggregated.weekly.info, MaxTrainingWeek = max(WeekNumber.ToPredict[idx.train]),
                               y = data.test$Yt, weeks = WeekNumber.ToPredict[idx.test], type = "truth")
     train.yhat = disaggregate(ADI = aggregated.weekly.info, MaxTrainingWeek = max(WeekNumber.ToPredict[idx.train]),
                               y = train.predictions, weeks = WeekNumber.ToPredict[idx.train], type = "prediction")
     test.yhat  = disaggregate(ADI = aggregated.weekly.info, MaxTrainingWeek = max(WeekNumber.ToPredict[idx.train]),
                               y = test.predictions, weeks = WeekNumber.ToPredict[idx.test], type = "prediction")
     
     
     # calculate the performance measures on the training and testing sets
     train.performance = get.regression.measures(truth = train.y, response = train.yhat)
     test.performance  = get.regression.measures(truth = test.y, response = test.yhat)
     names(train.performance) = paste0("train.", names(train.performance))
     names(test.performance)  = paste0("test.", names(test.performance))
     
     performance.results = rbind(performance.results,
                                 c(iter, train.performance, test.performance))
}
colnames(performance.results)[1] = "iter"

print(performance.results)
print(colMeans(performance.results[,-1]))
