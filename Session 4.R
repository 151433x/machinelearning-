#Session 4

# Pre-work
rm(list=ls())   # clear workspace, same as broom button under environment
gc()    # release occupied memory
cat("\f")     # clear Console, same as CTRL+L in Console

###################################
# MurphyBrothers.csv

MurphyBro.df <- read.csv("MurphyBrothers.csv")  # load data
View(MurphyBro.df) # show data frame
anyNA(MurphyBro.df)  # any missing values
str(MurphyBro.df) # structure: see variable names and types, 
# ts data are not in one column

t(names(MurphyBro.df))  # show column number of each variable
MurphyBro.df[2,3]  # call Feb of 1993
MurphyBro.df[2,] # call Feb of all years
MurphyBro.df[,3] # call all data from 1993
MurphyBro.df[1:3,] # call all data from Jan to March
MurphyBro.df[,3:5] # call all data from 1993 to 1995
MurphyBro.df[,c(2,5)] # call all data in 1992 and 1995
# c for concatenate.

# put data of the ts variable in one column
MB.tsdf <- unlist(MurphyBro.df[,2:5]) 
# convert second to 5th columns to one column and put them in the variable MB.tsdf
# or, use
MB.tsdf <- unlist(MurphyBro.df[,c(2,3,4,5)]) 

# Define a ts object
MB.ts <- ts(MB.tsdf, start = c(1992,1), frequency = 12) 
# convert data in MB.tsdf into a ts object; start from January of 1992
# frequency = 12, as it is the length of the seasonal pattern 
# This problem is in the case studies for Session 2
MB.ts # return the time series data with time stamps

MB.portion <- window(MB.ts, start=c(1993,1), end=c(1994,3))
# extract a portion (1993-Jan to 1994-Mar) of the ts data
MB.portion # return the time series portion with time stamps

install.packages("fpp2", dependencies = TRUE)  # install package
library("fpp2")  # load package

# ts plot
autoplot(MB.ts) 
autoplot(MB.ts) + 
  ggtitle("Murphy Brothers Sales") + 
  ylab("Sales") +
  xlab("Year") 
# ts plot with title, y label, and x label

# ts seasonal plot
ggseasonplot(MB.ts, year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Sales") + 
  ggtitle("Seasonal plot: Murphy Brothers Sales") 
# with year labels on both sides

# ts polar seasonal plot
ggseasonplot(MB.ts, polar=TRUE) + 
  ylab("Sales") +
  ggtitle("Polar seasonal plot: Murphy Brothers Sales")

# ts seasonal subseries plot
ggsubseriesplot(MB_ts) + ylab("Sales") + 
  ggtitle("Seasonal subseries plot: Murphy Brothers Sales")


###################################
# Adbudget.csv

rm(list=ls())   
gc()    
cat("\f")     

Adbudget.df <- read.csv("Adbudget.csv")  
View(Adbudget.df ) 
# Three time series there. Each time series is in one column/variable.
#Conversion not needed.
anyNA(Adbudget.df) 

str(Adbudget.df) # check variable types

Adbudget.ts  <- ts(Adbudget.df [ ,2:4], start=c(1981,1), frequency=4)
# turn Sales and AdBudget in Adbudget into a quarterly ts object;
# start from Q1 of 1981
Adbudget.ts

Adbudget.portion <- window(Adbudget.ts, start=c(1990,1), end=c(2000,4))
# extract a portion (1990-Q1 to 2000-Q4) of the ts data
Adbudget.portion

library("fpp2")  # load package

# ts plot with all ts variables
autoplot(Adbudget.ts) + 
  ggtitle("TS Plot") + 
  ylab("") + 
  xlab("Year")

# seasonal plots: one ts variable at a time
# ts seasonal plot
ggseasonplot(Adbudget.ts[,"Sales"], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Sales") + 
  ggtitle("Seasonal plot: Sales") 
# with year labels on both sides
# Adbudget.ts[,"Sales"] , not, Adbudget.ts$Sales 
# Adbudget.ts is a ts object

# ts polar seasonal plot
ggseasonplot(Adbudget.ts[,"Sales"], polar=TRUE) + 
  ylab("Sales") + 
  ggtitle("Polar seasonal plot: Sales")

# ts seasonal subseries plot
ggsubseriesplot(Adbudget.ts[,"Sales"]) + ylab("Sales") + 
  ggtitle("Seasonal subseries plot: Sales")

# scatterplot matrix
install.packages("GGally", dependencies = TRUE)  # install package
library("GGally")  # load package
ggpairs (as.data.frame (Adbudget.ts))
