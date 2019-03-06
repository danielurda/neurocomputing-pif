# Neurocomputing-BIP
This repository contains an example database and the R code to train a linear regression model using the t+1, t+7 and aggregated strategies covered in the paper.


# Pre-requisites
Both R and RStudio must be installed in your computer. Moreover, the following R libraries must be installed:
* caret
* hydroGOF
* mlr

If any of them is not installed yet, please proceed by installing them through the R command **install.packages('pck_name', dependencies=T)**. For example: **install.packages('caret', dependencies=T)**.

Additionally, set the R working directory to the directory where the code and data is located in your computer through the R command **setwd**, e.g. **setwd('/home/users/test/neurocomputing-pif/')**.


# Executing one example
There are three 3 R scripts which can be executed:
* example_t1.R: it performs a 10-fold cross-validation fitting a linear regression model using the **t+1** strategy.
* example_t7.R: it performs a 10-fold cross-validation fitting a linear regression model using the **t+7** strategy.
* example_aggr-disaggr.R: it performs a 10-fold cross-validation fitting a linear regression model using the **aggregated** strategy.

In any of these cases, first set the size of the autoregressive windows to be used by modifying the line 10 or 11 in the scripts, e.g. WINDOW=7.

A
