# Session 6 In-Class Practice

rm(list=ls())   
gc()    
cat("\f")  
library(fpp2) 

##########################
# Problem 1

ausbeer # show the ts object
anyNA(ausbeer) 

beer.ts <- window (ausbeer, start=c(1990,1))
# extract the data from Q1 of 1990
window (beer.ts, start=c(2000,2), end = c(2000,2))
# show data of Q2 of 2000

ggAcf(beer.ts, lag = 24) # correlogram 
# seasonality every 4 quarters

# Use Seasonal Naive Method
beer_sn <- snaive(beer.ts, 12)  
# return seasonal naive forecasts of 12 quarters into the future
beer_sn  # to see results of seasonal naive forecasts

autoplot(beer_sn) + ggtitle("Quarterly Production") + ylab("Sales") +
  xlab("Year") # ts plot with confidence intervals

##########################
# Problem 2

rm(list=ls())   
gc()    
cat("\f") 

eggs
anyNA(eggs) 

ggAcf(eggs, lag = 48) # correlogram 
# trend

eggs_Drift <- rwf(eggs, 7, drift=TRUE) 
# return drift forecasts of 7 years into the future
eggs_Drift # to see results of drift forecasts

autoplot(eggs_Drift) + ggtitle("Annual Egg Price") + ylab("Egg Price") +
  xlab("Year") # ts plot with confidence intervals

##########################
# Problem 3

pigs
anyNA(pigs) 

mypigs.ts <- window(pigs, start=c(1994,1)) 
# extract data from 1994
ggAcf(mypigs.ts, lag = 24) # correlogram 
# white noise

mypigs_Naive <- naive(mypigs.ts,16)  
# return naive forecasts of 16 months into the future
mypigs_Naive  # to see results of naive forecasts

autoplot(mypigs_Naive) + ggtitle("Monthly Number of Pigs Slaughtered") + 
  ylab("Number of Pigs Slaughtered") + xlab("Year") 
# ts plot with confidence intervals
