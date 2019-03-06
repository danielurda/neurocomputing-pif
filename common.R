library(hydroGOF)
library(mlr)


# given the truth outcome and response provided by a model, the function computes and return:
# - the Pearson's correlation coefficient
# - the index of agreement
# - the mean squared error
# - the mean absolute error
get.regression.measures <- function(truth, response) {
     res = c(cor(x = truth, y = response),
             hydroGOF::d(sim = response, obs = truth),
             measureMSE(truth = truth, response = response),
             measureMAE(truth = truth, response = response))
     
     names(res) = c("R","d","mse","mae")
     
     return(res)
}




# given the the PIF data and a size for the autoregressive windows, it will return
# a design matrix D={Xi,Yi} where:
# - Xi would be the W PIF counts in the past for the i-th sample
# - Yi would be the PIF count to be predicted in the next day for the i-th sample
timeseries.to.matrix.t1 <- function(X, W) {
     data  = NULL
     start.idx = 1
     for (i in (W+1):nrow(X)) {
          y = X$Count[i]
          x = rev(X$Count[start.idx:(i-1)])
          
          data = rbind(data, 
                       c(y, x, X$WeekNumber[i]))
          start.idx = start.idx+1
     }
     
     colnames(data) = c("Yt", paste0("Xt_",  1:(ncol(data)-2)), "WeekNumber.ToPredict")
     rownames(data) = NULL
     
     as.data.frame(data)
}




# given the the PIF data and a size for the autoregressive windows, it will return
# a design matrix D={Xi,Yi} where:
# - Xi would be the W/7 PIF counts in the past for the i-th sample in steps of 7 days
# - Yi would be the PIF to be predicted in 7 days for the i-th sample
timeseries.to.matrix.t7 <- function(X, W) {
     data  = NULL
     start.idx = 1
     for (i in (W+1):nrow(X)) {
          y = X$Count[i]
          x = rev(X$Count[seq(start.idx, i-1, by = 7)])
          
          data = rbind(data, 
                       c(y, x, X$WeekNumber[i]))
          start.idx = start.idx+1
     }
     
     colnames(data) = c("Yt", paste0("Xt_", seq(7, 7*(ncol(data)-2), by = 7)), "WeekNumber.ToPredict")
     rownames(data) = NULL
     
     as.data.frame(data)
}
