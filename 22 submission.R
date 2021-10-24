rm(list=ls())
gc()
cat('\f')
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/session 4/')

# QUESTION 2 40

df=read.csv('40numbers.csv')
View(df)

df.ts=ts(df,start=c(1,1),frequency=12)

ggAcf(df.ts,lag=24)# correlogram

#QUESTION 3 QUARTLY LOANS

df2=read.csv('QuarterlyLoans.csv')
Acf(df2,plot = FALSE)
ggAcf(df2)
qloans.ts=ts(df2,start = c(2001,1),frequency = 4)
autoplot(qloans)
# LAGS 1: .895 lAGS 2: .673

# this is not white noise. there is a trend here and there is seasonality.
# there is a trend but we dont know if its downawrd or upward
# however using autoplot we find that it is moving upward

#QUESTOIN 4

df3= read.csv('Elec_demand.csv')
df3=c(200.30 ,df3[,1])
df3.ts= ts(df3,start = c(1,1),frequency = 12)

ggAcf(df3.ts,maxlags=48)
autoplot(df3.ts)

ggsubseriesplot(df3.ts)
