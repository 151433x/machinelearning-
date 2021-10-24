# oct 4 submission
# saw sales
#pre work
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
#import the data and library
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 6/')
Sales.df=read.csv('SawsSales.csv')

#beer production of 19
ausbeer.window=window(ausbeer,start =c(1990,1))
#construct correllagram and identify time series variables.
#Trend and seasonal
test=tail(ausbeer.window,max(length(ausbeer.window)*.2,8))
train= head(ausbeer.window,length(ausbeer.window)-length(test))
#what naive model are you going to use to generate initial forecasts
beer.sn= snaive(train,length(test))
drift=rwf(train,8,drift = TRUE)
Naive=naive(train,length(test))

autoplot(drift)
autoplot(beer.sn)

# check residualas
driftAD=checkresiduals(drift,lag = 24)

# there is something wrong with seasonal as the residualas white noise
seasonalAD=checkresiduals(beer.sn,lag=24) # not adequeate
NaiveAD=checkresiduals(Naive)

accuracy(drift,test)

