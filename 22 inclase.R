# session 5 autocorrelational analysis 
#pre work clear work space,memories and console
rm(list=ls())
gc()
cat('\f')

library('fpp2')

#load the data set 
# surtido cookies.csv
setwd('C:/Users/xtorres1/Desktop/R/Datasets/session 4/'
)
df=read.csv('SurtidoCookies.csv')
#combine the columns

SurtdioCookies.ts=unlist(df[2:5])
Acf(SurtdioCookies.ts,lag.max = 24,plot = FALSE) # provides values of ACF's
ggAcf(SurtdioCookies.ts,lag=24)# correlogram

surtidoCookies.ts=ts(SurtdioCookies.ts,start = c(2000,1),end = c(2003,5),frequency=12)


