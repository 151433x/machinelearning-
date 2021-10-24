# ARIMA MODELS 10/18/21
#notes ish
# auto regressive integrated moving average (ARIMA)
# two types, seasonal and non-seasonal

#pre work
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 7/')
# step 1 non seasonal differecing: elimate trend from the ts data, keep only the stationary data
# determine differences.
three=read.csv('31.csv')
ql=read.csv('QuarterlyLoans.csv')
ggAcf(ql,lag=24) # not white noise and there is trend 
ql.ts=ts(ql,start = c(1,1),frequency = 1)

ggAcf(three,lag=24)# not white noise and there is trend
three.ts=ts(three,start = c(1,1),frequency = 1)

# try to get rid of trend in loans
ql.diff=diff(ql.ts)
ggAcf(ql.diff,lag=24) # got rid of trend with one differnce, d=1

# get rid of trend in three
three.diff=diff(three.ts)
ggAcf(three.diff)
t.diff=diff(three.diff)
ggAcf(t.diff)# got trend out with 2 differences, d=2

# ARIMA MODEL QUESTION2 

#105.2+.-535*72+.0055*99 
# 
