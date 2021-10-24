#sesssion 4 practice assignemnt
#Pre work

#clear enviorment and previous 
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
###
# annual obs
library('fpp2') # Load the packages

df=read.csv('C:/Users/xtorres1/Desktop/R/Datasets/session 4/2012.csv')
View(df)
anyNA(df)
str(df) #names of columns and datatypes
#multiples coluns, need to be handled

df.ts=ts(df,start = c(1,1),end = c(5,1),frequency = 1)# define TS value, Question A
df.portion=window(df.ts,start=c(2,1),end=c(3,1)) # portion of data for 2013 and 2014, question B

autoplot(df.ts)
# question C: Since there is no seasonality to the data all the graphs will error

# question 2

data=read.csv('C:/Users/xtorres1/Desktop/R/Datasets/session 4/SurtidoCookies.csv')
sum(is.na(data))
View(data) # in order to get a seasonality element to the data we have to get rid of NA's that are in our data so we cannot use all the data, we cant use the 4th y
data2=unlist(data[,2:5])# we get rid of the 4th year and use only our complete data
data3=ts(data2,start=c(2000,1),end=c(2003,5),frequency=12)             
autoplot(data3) # basic plot of 

ggseasonplot(data3,start=c(1,1),frequency=12) # we can make a seasonal plot with 2 years

ggseasonplot(data3,polar = TRUE,start(1,1),frequency=12) # we can make a polar plot

ggsubseriesplot(data3,ylab='sales',xlab='months')
