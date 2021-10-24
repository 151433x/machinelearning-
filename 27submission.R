# sept 27 submission
# saw sales
#pre work
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
#import the data and library
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/session 6/')
Sales.df=read.csv('SawsSales.csv')
View(ausbeer)

#beer production of 19
ausbeer.ts=ts(ausbeer,start =c(1990,1),frequency= 4)
#construct correllagram and identify time series variables.
ggAcf(ausbeer.ts)
#Trend and seasonal

#what naive model are you going to use to generate initial forecasts
beer.sn= snaive(ausbeer.ts,12)
drift=rwf(ausbeer.ts,20,drift = TRUE)
autoplot(drift)
autoplot(beer.sn)
# i used drift becasuse there was a clear trend going on with the data
# the forecast for 2013 q2 is 378.97

#question 2 
#prework
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
#import the data and library
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/session 6/')

anyNA(eggs)
eggs.ts=eggs

ggAcf(eggs.ts,lag=12)
# we should use drfit because there was a trend 
eggs_drift=rwf(eggs.ts,7,drift = FALSE)
autoplot(eggs_drift)

#question 3 
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
#import the data and library
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/session 6/')


