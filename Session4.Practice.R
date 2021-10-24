# Session 4: Practice

rm(list=ls())   
gc()    
cat("\f")     

library(fpp2)

###################
# Annual Observations from 2012.csv

AnnualOb.df <- read.csv("Annual Observations from 2012.csv")  
View(AnnualOb.df) 
anyNA(AnnualOb.df)  
str(AnnualOb.df) 
# ts data are in one column

AnnualOb.ts <- ts(AnnualOb.df$Ob, start=c(2012,1), frequency = 1)  
# define the time series data to a ts object, AnnualOb_ts; 
# annual data, only 1 obs per year
AnnualOb.ts
# return the ts object

AnnualOb.ts_13to14 <- window(AnnualOb.ts, start=c(2013,1), end=c(2014,1))
# extract a portion (2013 and 2014) of the ts object
AnnualOb.ts_13to14
# return the portion of the ts object 

# ts plot
autoplot(AnnualOb.ts) + ggtitle("Annual Observations") + ylab("Observations") +
  xlab("Year") # ts plot with title, y label, and x label

ggseasonplot(AnnualOb.ts, year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Observations") + 
  ggtitle("Seasonal plot") 
# Error, not seasonal data

################
#SurtidoCookies.xlsx

rm(list=ls())   
gc()    
cat("\f")  

SC.df <- read.csv("SurtidoCookies.csv")  
View(SC.df) 
anyNA(SC.df)  # missing values are fine
str(SC.df)  
# ts data are not in one column

# convert ts data to one column
SC.tsdf  <- unlist(SC.df[,2:5]) 
SC.ts  <- ts(SC.tsdf, start=c(2000,1), end = c(2003,5), frequency=12) 
# convert data in SC_ts into a ts object; start from January of 2000
SC.ts

# ts plot
autoplot(SC.ts) + ggtitle("Surtido Cookies Sales") + ylab("Sales") +
  xlab("Year") 

# ts seasonal plot
ggseasonplot(SC.ts, year.labels=TRUE, year.labels.left=TRUE) + ylab("Sales") + 
  ggtitle("Seasonal plot: Surtido Cookies Sales") 

# ts polar seasonal plot
ggseasonplot(SC.ts, polar=TRUE) + ylab("Sales") +
  ggtitle("Polar seasonal plot: Surtido Cookies Sales")

# ts seasonal subseries plot
ggsubseriesplot(SC.ts) + ylab("Sales") + 
  ggtitle("Seasonal subseries plot: Surtido Cookies Sales")

