# Session 7 In-Class Practice- Problem 1

##########################
# Problem 1

rm(list=ls())   
gc()    
cat("\f")  
library(fpp2) 

ausbeer # show the ts object
beer.ts <- window (ausbeer, start=c(1990,1))
# extract the data from Q1 of 1990

test <- tail(beer.ts,max(length(beer.ts)*0.2, 8)) 
# create the test set, size: bigger one of length(beer)*0.20 and 8
test

training <- head(beer.ts,length(beer.ts) - length(test))
training

sn <- snaive(training, length(test))  
# return seasonal naive forecasts of 12 quarters into the future
checkresiduals (sn) # produce plots to check adequacy 
# not adequate

Naive <- naive(training, length(test))  
# apply naive method to predict values on test set
checkresiduals (Naive) # produce plots to check adequacy 
# not adequate

Drift <- rwf(training, length(test), drift=TRUE) 
# apply drift method to predict values on test set
checkresiduals (Drift) # produce plots to check adequacy 
# not adequate

accuracy(Naive,test)[2,] # return forecast errors
accuracy(Drift,test)[2,]
accuracy(sn,test)[2,]

# according to MAPE values, use Seasonal Naive Method
predictions <- snaive(beer.ts, 8)
predictions

autoplot(predictions) + autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Production") + ylab("Sales") +
  xlab("Year") # ts plot with confidence intervals


##########################
# Problem 2

rm(list=ls())   
gc()    
cat("\f") 
library(fpp2) 

eggs
test <- tail(eggs,max(length(eggs)*0.2, 5)) 
# create the test set, size: bigger one of length(beer)*0.20 and 8
test

training <- head(eggs,length(eggs) - length(test))
# create the training set, all data except the 4 ts data at the end
training

Drift <- rwf(training, length(test), drift=TRUE) 
# apply drift method to predict values on test set
checkresiduals (Drift) # produce plots to check adequacy 
# not adequate

Naive <- naive(training, length(test))  
# apply naive method to predict values on test set
checkresiduals (Naive) # produce plots to check adequacy 
# not adequate

sn <- snaive(training, length(test))  
# return seasonal naive forecasts of 12 quarters into the future
checkresiduals (sn) # produce plots to check adequacy 
# not adequate

accuracy(Naive,test)[2,]# return forecast errors
accuracy(Drift,test)[2,] 
accuracy(sn,test)[2,] 

# according to MAPE values, use Drift Method
predictions <- rwf(eggs, 5, drift=TRUE)  
predictions

autoplot(predictions) + autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Annual Egg Price") + ylab("Egg Price") +
  xlab("Year") # ts plot with confidence intervals

