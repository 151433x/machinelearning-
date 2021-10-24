# saw sales
#pre work
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
#import the data and library
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/session 6/')
Sales.df=read.csv('SawsSales.csv')
View(sales.df)
anyNA(Sales.df)
ggAcf(Sales.df,lag.max = 24)
# can tell seasonality using correlogram
sales.ts=ts(Sales.df,start = c(2002,1),frequency = 4)

#naive method implementation

Naive=naive(sales.ts,8) #predict for 8 quarters into hte future
autoplot(Naive)

#drift method
#goog200 already definied as a ts object
drift= rwf(sales.ts)

goog200

ggAcf(goog200,lag=24)
# correlagram 

drift=rwf(goog200,20,drift = TRUE)
autoplot(drift)+ggtitle('historical and forecasts of stoc prices')+ylab('google price')+xlab('day')

# seasonal naive method

SN=snaive(sales.ts,8)
autoplot(SN)
