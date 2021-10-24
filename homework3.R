# homework 3, Xavier Torres
#part1
# prework
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 7/')
#question 1
df=read.csv('annual.csv')
str(df)
# there is only one element that is a time series varaible 
#question 2 
df=df[order(df$Year,decreasing = FALSE),]
df$Year=NULL
df.ts=ts(df,start = c(1,1),frequency = 1)
ggAcf(df.ts,lag=100)# there is trend and there is no seasonality 
#question 3 
autoplot(df.ts) # there is a definite upward trend in the data
#setting up training and testing set

test=tail(df.ts,max(length(df.ts)*.2,10))
train= head(df.ts,length(df.ts)-length(test))

#question 4 

naive.ts=naive(train,length(test))# naive
drift.ts=rwf(train,length(test))# drift method
snaive.ts=snaive(train,length(test))#snaive method
meanf.ts=meanf(train,length(test))# simple average
ses.ts=ses(train,length(test))# exponential smoothing
holt.ts=holt(train,length(test))#holt winters method

# check residuals 

checkresiduals(naive.ts)# white noise residuals, which 
checkresiduals(drift.ts)# white noise 
checkresiduals(snaive.ts)#white noise 
checkresiduals(meanf.ts)# not white noise, which means that it is not able to be used, there is a trend in the data
checkresiduals(ses.ts)  #white noise 
checkresiduals(holt.ts) #white noise 

#accuracies 

accuracy(meanf.ts,test)[2,]
accuracy(naive.ts,test)[2,]
accuracy(snaive.ts,test)[2,]
accuracy(ses.ts,test)[2,]
accuracy(holt.ts,test)[2,]

#prediction

predictions=holt(df.ts,10)
autoplot(predictions)+ggtitle('global scale temperture')+xlab('years away from 1800')

#part 2 
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 7/')
df=read.csv('myTS.csv')

#question 1 
anyNA(df)

#question 2
str(df)
df$X=NULL
df.adbudget=df[,2]

#question 3 

ggAcf(df.adbudget,lag=24)# trend and sesonal, frequency of 4

adbudget.ts=ts(df.adbudget,start = c(1,1),frequency = 4)

#question 4 

#train and test set up
test.adbudget=tail(adbudget.ts,max(length(adbudget.ts)*.2,5))
train.adbudget= head(adbudget.ts,length(adbudget.ts)-length(test.adbudget))

#models

naive.ad.ts=naive(train.adbudget,length(test.adbudget))# naive
drift.ad.ts=rwf(train.adbudget,length(test.adbudget))# drift method
snaive.ad.ts=snaive(train.adbudget,length(test.adbudget))#snaive method
meanf.ad.ts=meanf(train.adbudget,length(test.adbudget))# simple average
ses.ad.ts=ses(train.adbudget,length(test.adbudget))# exponential smoothing
sesf.ad.ts=ses(train.adbudget,length(test.adbudget),alpha = .5,intial='optimal')# exponential smoothing
holt.ad.ts=holt(train.adbudget,length(test.adbudget))#holt winters method basic
hwa.ad.ts=hw(train.adbudget,length(test.adbudget),seasonal = 'additive')#holt winters method basic
hwm.ad.ts=hw(train.adbudget,length(test.adbudget),seasonal = 'multiplicative')#holt winters method basic

# check residuals

checkresiduals(naive.ad.ts) # this model is not accurate as there is not white noise in the residuals, bad
checkresiduals(drift.ad.ts)# this model is not accurate as there is not white noise in residuals, bad
checkresiduals(snaive.ad.ts)# this model is more accurate than the others as it takes seasons in too considerations but still have does not have white noise in residuals
checkresiduals(meanf.ad.ts)# this model is not accurate as it does not account for seasonality, bad
checkresiduals(ses.ad.ts) # this model is not accurate as it does not account for seasonality, bad
checkresiduals(sesf.ad.ts)# this model also does not account for seasonality so it fails , bad 
checkresiduals(holt.ad.ts)#tihs model also does not account for seasonlity, bad
checkresiduals(hwm.ad.ts)# this method does well as it takes account for seasonality
checkresiduals(hwa.ad.ts)# this method also does better than the last but only marginally.

#predictions
predictions.hwa=hw(adbudget.ts,seasonal = 'additive',5)

predictions.hwm=hw(adbudget.ts,seasonal = 'multiplicative',5)

autoplot(predictions.hwm)+ggtitle('adbudget by quarter using holt winter multiplicative')+autolayer(fitted(predictions.hwm),series = 'fitted')

#accuracies
accuracy(predictions.hwm)
