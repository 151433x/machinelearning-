# Session 8

rm(list=ls())   
gc()    
cat("\f")  
library(fpp2) 

Sales.df <- read.csv("SawsSales.csv") 
anyNA(Sales.df) 
str(Sales.df)  

ggAcf(Sales.df,lag=24)
# seasonality, quarterly

Sales.ts <- ts(Sales.df, start=c(2002,1), frequency = 4) 
Sales.ts

# Step 1 partitioning

test <- tail(Sales.ts, max(length(Sales.ts)*0.2, 4)) 
test

training <- head(Sales.ts,length(Sales.ts) - length(test))
training

# Step 2&3 forecast on test; residual check on training

# Simple avg
Avg <- meanf(training, length(test))  
# apply simple average method to predict values on test set
Avg
checkresiduals (Avg) 
# adequate

# exp smoothing
ses <- ses(training, length(test)) 
# apply exponential smoothing method to predict values on test set
# with "optimal" alpha and initial forecast
ses
checkresiduals (ses) 
# not adequate

# exp smoothing with alpha = 0.1
Ses0.1 <- ses(training, alpha = 0.1, length(test)) 
# apply exponential smoothing method to predict values on test set
# with alpha = 0.1, and "optimal" initial forecast
Ses0.1
checkresiduals (Ses0.1)  
# adequate

# exp smoothing with alpha = 0.3
Ses0.3 <- ses(training, alpha = 0.3, length(test)) 
Ses0.3
checkresiduals (Ses0.3)  
# not adequate
# can continue to try other alphas

# HW methods

Holt <- holt(training, length(test)) 
# apply Holt method to predict values on test set
# with "optimal" parameters
Holt
checkresiduals (Holt) # produce plots to check adequacy 
#not adequate

HoltWinters_A <- hw(training, seasonal = "additive", length(test)) 
# apply HW Additive method to predict values on test set
# with "optimal" parameters
HoltWinters_A
checkresiduals (HoltWinters_A) 
#adequate

HoltWinters_M <- hw(training, seasonal = "multiplicative", length(test)) 
# apply Holt-Winters Multiplicative method to predict values on test set
# with "optimal" parameters
HoltWinters_M
checkresiduals (HoltWinters_M) # produce plots to check adequacy 
#adequate

# Step 4: performance of the adequate methods

accuracy(Avg,test)[2,] 
accuracy(Ses0.1,test)[2,]
accuracy(HoltWinters_A,test)[2,]
accuracy(HoltWinters_M,test)[2,]

# If MAPE is the selected error, HoltWinters_M is the best method  
# apply HoltWinters_M to predict future values
predictions <- hw(Sales.ts, seasonal = "multiplicative", 4) 
predictions

autoplot(predictions) + autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Sales") + ylab("Sales") +
  xlab("Year") # ts plot with confidence intervals
