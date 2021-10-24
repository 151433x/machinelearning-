#############################This program was created by Davit Khachatryan##########################################
######©2019 by Davit Khachatryan.  All rights reserved. No part of this document may  be reproduced or##############
######transmitted in any form or by any means, electronic, mechanical, photocopying, recording or otherwise#########
############################without prior written permission of Davit Khachatryan###################################
####################################################################################################################


library(h2o)
library(gmodels)


#Initialize the h2o cluster
h2o.init(nthreads = -1, max_mem_size = "4g")

mydata.train=read.csv("C:/Users/dkhachatryan/Documents/Teaching/Spring 2021/Independent Study/rawdata/mnist_train.csv")
mydata.test=read.csv("C:/Users/dkhachatryan/Documents/Teaching/Spring 2021/Independent Study/rawdata/mnist_test.csv")

#Plot some of the digits below

m=matrix(unlist(mydata.test[21,-1]), nrow=28, byrow = T) #Specifies to plot the image on the 21st row of test set

image(m, col=grey.colors(255))

rotate=function(x) t(apply(x,2,rev)) #Is a function to rotate an image

image(rotate(m), col=grey.colors(255))


#START OF VARIABLE REDEFINITION

mydata.train$myresponse=mydata.train$label #Substitute "quality" with the name of your response variable
mydata.train$label=NULL #Substitute "quality" with the name of your response variable

mydata.test$myresponse=mydata.test$label #Substitute "quality" with the name of your response variable
mydata.test$label=NULL #Substitute "quality" with the name of your response variable

mydata=rbind(mydata.train,mydata.test)

#END OF VARIABLE REDEFINITION

#How many 0s, 1s, 2s, etc. do we have?
table(mydata$myresponse)


#Convert the outcome variable to a factor
mydata.train$myresponse=as.factor(mydata.train$myresponse)
mydata.test$myresponse=as.factor(mydata.test$myresponse)


#Below in "hid.list" you need to specify the topology of the candidate networks to fit. Each
#network is identified by the number of hidden layers and nodes per hidden layer.
#each entry below is 1 network - e.g. c(2) is a network with 1 hidden layer with 2 nodes; c(2,2) is a network with 
#2 hidden layers with 2 node in each layer, etc. 

hid.list=list(c(15))


str(mydata)


#Designate the list of candidate parameters to optimize as part of a grid search later on in the program
#Note that we are going to tune the regularization (LASSO) parameter only, but technically other parameters
#can be tuned similarly by being passed to the below list. The list below is going to be used in the so-called 
#"grid search" to find the parameter(s) that are "optimal" based on a certain criteria (e.g. mape, misclassification error, etc.)

hyper_params <- list(
  hidden=hid.list,
  l1=c(0,0.001, 0.05)
)


#This vector will contain the names of all predictors
predictors <- setdiff(names(mydata.train), "myresponse")

#To use H2o, we need to convert each dataframe (both train and test) to h2o format. That is done below.
mydata.train.for.h2o=as.h2o(mydata.train)
mydata.test.for.h2o=as.h2o(mydata.test)

#The configuration below sets up an empty table that will ultimately contain the results of the grid search, 
#i.e. information on each model that has been tried and the corresponding statistics like error rates, etc.

set.seed(NULL)
id=paste("mygrid", round(runif(1,1,10000)), sep="")

grid <- h2o.grid(
  
  algorithm="deeplearning", #this specifies which algorithm/method of h2o is being run
  grid_id=id, 
  training_frame=mydata.train.for.h2o,
  nfolds=10,                #this specifies the number of folds (k) for k-fold cross-validation
  
  x=predictors, 
  y="myresponse",
  
  use_all_factor_levels=TRUE,
  standardize = TRUE,       #we are instructing h2o to standardize the numeric predictors
  
  stopping_metric="misclassification",
  stopping_tolerance=1e-2,      
  stopping_rounds=2,        #This and the previous two metrics tell h2o to stop when the average error does not improve beyond 
                            #the stopping_tolerance threshold for stopping_rounds number of rounds
  
  activation=c("Rectifier"),#This sets up the type of activation function to be used in hidden layers
  epochs=10,
  hyper_params=hyper_params,#This tells h2o to try each model implied by the parameters specified earlier in hyper_params list
  
  overwrite_with_best_model=TRUE,
  export_weights_and_biases=TRUE
  
)

grid.summary <- h2o.getGrid(id,sort_by="err",decreasing=FALSE)

View(grid.summary@summary_table)

#Getting the best model

grid.summary@summary_table[1,]
best_model <- h2o.getModel(grid.summary@model_ids[[1]])
best_model



#Predictive accuracy for the best model (evaluating on the test set)
#Note that for classification the class with the highest probability gets assigned as the prediction

#Predict the testing set first
predictions=h2o.predict(best_model, mydata.test.for.h2o)
predictions=as.data.frame(predictions)

classes=colnames(predictions)[2:ncol(predictions)]
predictions$tt=apply(predictions[,2:ncol(predictions)],1,which.max)
f<-function(i) classes[i]
predictions$predict=sapply(predictions$tt,f)

mydata.test.with.pred=cbind(mydata.test, predictions)
if (substr(mydata.test.with.pred$myresponse[1],1,1)!="p" & sum(substr(mydata.test.with.pred$predict,1,1)=="p")==nrow(mydata.test.with.pred)){
  mydata.test.with.pred$predict=gsub("p","",mydata.test.with.pred$predict)}
  
CrossTable(mydata.test.with.pred$myresponse,mydata.test.with.pred$predict,prop.chisq=F,prop.t=F)
  
#Correct Classification Rate
sum(mydata.test.with.pred$myresponse==mydata.test.with.pred$predict)/nrow(mydata.test.with.pred)

#See some of the digits that were mis-classified - e.g. the in the testing set the 9th observation where a 5 is mis-classified as 6

#Let's print it

m=matrix(unlist(mydata.test[9,-1]), nrow=28, byrow = T)

rotate=function(x) t(apply(x,2,rev))

image(rotate(m), col=grey.colors(255))
