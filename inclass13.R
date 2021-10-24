# Session 8

rm(list=ls())   
gc()    
cat("\f")  
library(fpp2) 
#ausbeer

#create window

ausbeer.window=window(ausbeer,start=c(1990,1))
ggAcf(ausbeer.window)
# Step 1 partitioning
test <- tail(ausbeer.window, max(length(ausbeer.window)*0.2, 4)) 
test

training <- head(ausbeer.window,length(ausbeer.window) - length(test))
training

# avg smoothing

ausbeer.simple=meanf(training,length(test))

#exp smoothing

ausbeer.ses3= ses(training,alpha=.3,length(test))
ausbeer.ses5=ses(training,alpha=.5,length(test))
ausbeer.ses=ses(training,length(test))

#hw smoothing

ausbeer.holt=holt(training,length(test))

ausbeer.hw=hw(training,seasonal="additive",length(test))

ausbeer.hwm=hw(training,seasonal="multiplicative",length(test))


#check residuals

checkresiduals(ausbeer.simple) # not acceptable
checkresiduals(ausbeer.ses) # not acceptable
checkresiduals(ausbeer.ses3)# not acceptable
checkresiduals(ausbeer.ses5)# not acceptable
checkresiduals(ausbeer.holt)# not acceptable
checkresiduals(ausbeer.hw)  # not acceptable
checkresiduals(ausbeer.hwm) # not acceptable
# check accuracy of acceptables
accuracy(ausbeer.hw,test)[2,]#rmse of 9.165
accuracy(ausbeer.hwm,test)[2,]#rmse of 8.855

#forecast
predictions.ausbeer <- hw(ausbeer.window, seasonal = "multiplicative", 8) 
predictions

autoplot(predictions.ausbeer) + autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Sales") + ylab("Sales") +
  xlab("Year") # ts plot with confidence intervals


##eggs
is.ts(eggs)
ggAcf(eggs)
# Step 1 partitioning
test <- tail(eggs, max(length(eggs)*0.2, 4)) 
test

training <- head(eggs,length(eggs) - length(test))
training

# avg smoothing

eggs.simple=meanf(training,length(test))

#exp smoothing

eggs.ses3= ses(training,alpha=.3,length(test))
eggs.ses5=ses(training,alpha=.5,length(test))
eggs.ses=ses(training,length(test))

#hw smoothing

eggs.holt=holt(training,length(test))

eggs.hw=hw(training,seasonal="additive",length(test))# wont work as there is no frequency

eggs.hwm=hw(training,seasonal="multiplicative",length(test))# wont work as there is no frequency


#check residuals

checkresiduals(eggs.simple) #  acceptable
checkresiduals(eggs.ses) # not acceptable white noise 
checkresiduals(eggs.ses3)#  acceptable
checkresiduals(eggs.ses5)#  acceptable
checkresiduals(eggs.holt)# not acceptable white noise 
checkresiduals(eggs.hw)  # not acceptable
checkresiduals(eggs.hwm) # not acceptable
# check accuracy of acceptables
accuracy(eggs.simple,test)[2,]#RMSE: 142
accuracy(eggs.ses3,test)[2,]#RMSE:58
accuracy(eggs.ses5,test)[2,]#RMSE:62

#predictions

predictions.eggs <- ses(eggs,alpha =.3, 5) 
predictions

autoplot(predictions.eggs) + autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Sales") + ylab("Sales") +
  xlab("Year") # ts plot with confidence intervals


