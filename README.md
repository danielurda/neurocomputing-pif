# Neurocomputing-BIP
This repository contains an example database corresponding to the library users at the Polytechnic School of Engineering in Algeciras (Spain), which has a weekly periodicity. Moreover, it provides the R code to train a linear regression model using the t+1, t+7 and aggregated strategies covered in the paper.


# Pre-requisites
Both R and RStudio must be installed in your computer. Moreover, the following R libraries must be installed:
* caret
* hydroGOF
* mlr

If any of them is not installed yet, please proceed by installing them through the R command **install.packages**, e.g. **install.packages('caret', dependencies=T)**.

Additionally, set the R working directory to the directory where the code and data is located in your computer through the R command **setwd**, e.g. **setwd('/home/users/test/neurocomputing-pif/')**.


# Executing one example
There are three 3 R scripts which can be executed:
* example_t1.R: it performs a 10-fold cross-validation fitting a linear regression model using the **t+1** strategy.
* example_t7.R: it performs a 10-fold cross-validation fitting a linear regression model using the **t+7** strategy.
* example_aggr-disaggr.R: it performs a 10-fold cross-validation fitting a linear regression model using the **aggregated** strategy.

In any of these cases, first set the size of the autoregressive windows to be used in the analysis by modifying line 10 of the scripts, e.g. **WINDOW=7**.

Then, simply execute the script through the R command **source**, e.g. **source('example_t1.R')**.

The execution of these scripts will show the train and test performance measures computed for each iteration of the 10-fold cross-validation evaluation strategy as well as the average performance.
