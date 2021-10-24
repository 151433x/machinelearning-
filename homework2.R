# homework 2 
#pre work
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
#import the data and library
library('fpp2')

setwd('C:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 7/')
df=read.csv('Sealevel.csv')

# are there any missing values: 
anyNA(df)

# how many time serires variables are in the data set: there is only one variable in hte data set
View(df)
str(df)
df$Year=NULL
df$Month=NULL
df.ts=ts(df,start = c(1,1),frequency = 12)
ggAcf(df.ts,lag=1200)
autoplot(df.ts)+ylab('relative sea level')+ggtitle('rising sea levels over the years')
