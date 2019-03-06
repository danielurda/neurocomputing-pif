# given the raw PIF data, it buils a matrix where each row contains aggregated information for every single week:
# - WeekNumber: 1, 2, 3, .... 
# - Wt: sum of the daily PIF counts for the given week
# - Yt: daily PIF counts for the given week
# - Bt: coefficients representing the ratio between the daily PIF counts and the sum of the daily PIF counts
aggregate.weekly.info <- function(X) {
     aggregated.weeks = NULL
     for (i in 1:max(X$WeekNumber)) {
          daily.pif.per.week     = rev(X$Count[X$WeekNumber == i])
          sum.daily.pif.per.week = sum(daily.pif.per.week)
          daily.weight.per.week  = daily.pif.per.week/sum.daily.pif.per.week
          
          aggregated.weeks        = rbind(aggregated.weeks, 
                                         c(i, sum.daily.pif.per.week, daily.pif.per.week, daily.weight.per.week))
     }
     colnames(aggregated.weeks) = c("WeekNumber","Wt", paste0("Yt_", 1:7), paste0("Bt_", 1:7))
     rownames(aggregated.weeks) = NULL
     
     as.data.frame(aggregated.weeks)
}




# given the aggregated weekly information of the PIF data and a size for the autoregressive windows, it will return
# a design matrix D={Xi,Yi} where:
# - Xi would be the W/7 aggregated values in the past for the i-th sample
# - Yi would be the next aggregated value (the entire next week) to be predicted for the i-th sample
aggregated.timeseries.to.matrix <- function(X, W) {
     data  = NULL
     start.idx = 1
     for (i in ((W/7)+1):nrow(X)) {
          y = X$Wt[i]
          x = rev(X$Wt[start.idx:(i-1)])
          
          data = rbind(data, 
                       c(y, x, i))
          start.idx = start.idx+1
     }
     
     colnames(data) = c("Yt", paste0("Xt_",  1:(ncol(data)-2)), "WeekNumber.ToPredict")
     rownames(data) = NULL
     
     as.data.frame(data)
}




# it can be called in two modes:
# - when type is truth, it converts the aggregated values (sum of daily PIF counts in a week) back to the original 
#   vector of daily PIF counts
# - when type is prediction, it takes the Bt coefficients only for weeks used to train the model from the aggregated 
#   weekly information matrix, it computes the median value for each Bt coefficient and the result is used to weight the
#   predicted aggregated value of the model
disaggregate <- function(ADI, MaxTrainingWeek, y, weeks, type=c("truth","prediction")) {
     type = type[1]
     
     if (type == "truth") {
          y.disaggr = lapply(weeks, function(w) rev(unlist(ADI[ADI$WeekNumber==w, grep("Yt_", colnames(ADI))])))
          y.disaggr = as.numeric(do.call(c, y.disaggr))
     } else {
          # when making predictions of the future, only information of the training (past time) will be available
          ADI = ADI[ADI$WeekNumber <= MaxTrainingWeek,]
          
          disaggregation.coefs = lapply(weeks, function(w) apply(ADI[ADI$WeekNumber<w, grep("Bt", colnames(ADI))], 2, median))
          disaggregation.coefs = do.call(rbind, disaggregation.coefs)
          y.disaggr            = lapply(1:length(y), function(i) rev(y[i]*disaggregation.coefs[i,]))
          y.disaggr            = as.numeric(do.call(c, y.disaggr))
     }
     
     y.disaggr
}