# session 7 inclass
rm(list=ls())
gc()
cat('\f')
library('fpp2')

#prework
setwd("C:/Users/xtorres1/Desktop/R/Datasets/session 6/")
sales.df=read.csv('SawsSales.csv')
str(sales.df)
anyNA(sales.df)
#convert to ts object
ggAcf(sales.df,lag=24)
# seasonality is 4 
sales.ts=ts(sales.df,start = c(2002,1),frequency = 4)
#### step 1: data partition
#### step2: predict on test 
#### step3: check adequacy on training
#### step4: performance assessment

# predict for future 
# step 1 and 2 
test=tail(sales.ts,max(length(sales.ts)*.2,4))
# set up the test data set
train= head(sales.ts,length(sales.ts)-length(test))
# set up the training data set
Naive=naive(train,length(test))
drifttrain=rwf(train,length(test),drift = TRUE)
seasonaltrain=snaive(train,length(test))
# adequacy check 
# using residuals from TRAINING(IT MUST BE TRAINIING OR ITS OVERFITTING)
# checking residuals
checkresiduals(Naive)# bad
checkresiduals(drifttrain)# not adequete
checkresiduals(seasonaltrain)#not adequete but better
# step 4 
# general metricts for error: MAE,RMSE,MAPE,MPE
