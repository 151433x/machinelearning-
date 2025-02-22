#############################This program was created by Davit Khachatryan##########################################
######�2019 by Davit Khachatryan.  All rights reserved. No part of this document may  be reproduced or##############
######transmitted in any form or by any means, electronic, mechanical, photocopying, recording or otherwise#########
############################without prior written permission of Davit Khachatryan###################################
####################################################################################################################



library(h2o)
library(gmodels)
library(lime)


#Initialize the h2o cluster
h2o.init(nthreads = -1, max_mem_size = "4g")

mydata=read.csv("C:/Users/xtorres1/Desktop/R/Datasets/Meth Machine learning/Car MPG Data.csv")

#Is this a regression or classification problem?

problem.type="R" #Enter "C" for classification and "R" for regression


#START OF VARIABLE REDEFINITION

mydata$myresponse=mydata$mpg #Substitute "quality" with the name of your response variable
mydata$mpg=NULL #Substitute "quality" with the name of your response variable

str(mydata)

#END OF VARIABLE REDEFINITION

#Run the next 3 lines without modifications
if (problem.type=="C")
table(mydata$myresponse)else
  hist(mydata$myresponse)


#The statements below remove all the variables that will not be passed to the algorithm
#as predictors. If no such redundant variables exist in your dataset, then the statements
#in the "REDUNDANT VARIABLE REMOVAL" section should be deleted or put after a hashtag.

#START OF REDUNDANT VARIABLE REMOVAL

mydata$car.name=NULL

#END OF REDUNDANT VARIABLE REMOVAL


#In the following statements substitute the names after "$" sign with the names of prdictors
#in your data that are categorical but are read into R in a different format. If there are no such 
#variables in your data, then ignore.

#START OF PREDICTOR TRANSFORMATION

mydata$origin=as.factor(mydata$origin)
#mydata$xyz=as.factor(mydata$xyz)

#add statements similar to above as needed

#END OF PREDICTOR TRANSFORMATION

#START OF RESPONSE TRANSFORMATION

#Un-comment the line below only if the response needs to be converted to categorical(factor)

#mydata$myresponse=as.factor(mydata$myresponse)

#END OF RESPONSE TRANSFORMATION


#Designate all categorical variables (including the dependent variable if doing classification),
#Separated by commas. If you specified type="C" above then "myresponse" SHOULD be included
#in "cat.vars" below. If there are no categorical variables, "cat.vars" should be an empty vector, 
#i.e. cat.vars=c()

cat.vars=c("origin")


#Below in "hid.list" you need to specify the topology of the candidate networks to fit. Each
#network is identified by the number of hidden layers and nodes per hidden layer.
#each entry below is 1 network - e.g. c(2) is a network with 1 hidden layer with 2 nodes; c(2,2) is a network with 
#2 hidden layers with 2 node in each layer, etc. 

hid.list=list(c(4,3,4))


str(mydata)


#Designate the list of candidate parameters to optimize as part of a grid search later on in the program
#Note that we are going to tune the regularization (LASSO) parameter only, but technically other parameters
#can be tuned similarly by being passed to the below list. The list below is going to be used in the so-called 
#"grid search" to find the parameter(s) that are "optimal" based on a certain criteria (e.g. mape, misclassification error, etc.)


hyper_params <- list(
  hidden=hid.list,
  l1=c(0,0.001, 0.05),
  l2=c(0,0.001,0,05,.01,.02,.03)
)


#START DATA BREAKDOWN FOR HOLDOUT METHOD

#Find the number of categorical predictors first

numpredictors=dim(mydata)[2]-1

numfac=0

for (i in 1:numpredictors) {
  if ((is.factor(mydata[,i]))){
    numfac=numfac+1} 
}


#End finding the number of categorical predictors 

nobs=dim(mydata)[1]


if (problem.type=="R") {
  
  #Below is the setup for stratified 80-20 holdout sampling 
  
  train_size=floor(0.8*nobs)
  test_size=nobs-train_size
  
} else {
  
  #Below is the setup for stratified 80-20 holdout sampling 
  
  prop = prop.table(table(mydata$myresponse))
  length.vector = round(nobs*0.8*prop)
  train_size=sum(length.vector)
  test_size=nobs-train_size
  class.names = as.data.frame(prop)[,1]
  numb.class = length(class.names)}


resample=1
RNGkind(sample.kind = "Rejection")
set.seed(1) #sets the seed for random sampling

while (resample==1) {
  
  
  if (problem.type=="C") {
    
    train_index = c()
    
    for(i in 1:numb.class){
      index_temp = which(mydata$myresponse==class.names[i])
      train_index_temp = sample(index_temp, length.vector[i], replace = F)
      train_index = c(train_index, train_index_temp)
    }} else {
      train_index=sample(nobs,train_size, replace=F)
    }
  
  mydata.train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
  mydata.test=mydata[-train_index,]#everything not in the training set should go into testing set
  
  right_fac=0 #denotes the number of factors with "right" distributions (i.e. - the unique levels match across mydata, test, and train data sets)
  
  
  for (i in 1:numpredictors) {
    if (is.factor(mydata.train[,i])) {
      if (sum(as.vector(unique(mydata.test[,i])) %in% as.vector(unique(mydata.train[,i])))==length(unique(mydata.test[,i])))
        right_fac=right_fac+1
    }
  }
  
  if (right_fac==numfac) (resample=0) else (resample=1)
  
}

dim(mydata.test) #confirms that testing data has only 20% of observations
dim(mydata.train) #confirms that training data has 80% of observations


#END DATA BREAKDOWN FOR HOLDOUT METHOD

#Start grid search
predictors <- setdiff(names(mydata.train), "myresponse")

#To use H2o, we need to convert each dataframe (both train and test) to h2o format. That is done below.
mydata.train.for.h2o=as.h2o(mydata.train)
mydata.test.for.h2o=as.h2o(mydata.test)

#The configuration below sets up an empty table that will ultimately contain the results of the grid search, 
#i.e. information on each model that has been tried and the corresponding statistics like error rates, etc.

set.seed(NULL)
id=paste("mygrid", round(runif(1,1,10000)), sep="")


#NOTE: Running single-threaded for reproducibility. If want to run multi-threaded, 
#change to "reproducible=F" and get rid of the seed in the function below.

grid <- h2o.grid(
  
  algorithm="deeplearning", #this specifies which algorithm/method of h2o is being run
  grid_id=id, 
  training_frame=mydata.train.for.h2o,
  nfolds=10, #this specifies the number of folds (k) for k-fold cross-validation
  
  x=predictors, 
  y="myresponse",
  
  use_all_factor_levels=TRUE,
  standardize = TRUE, #we are instructing h2o to standardize the numeric predictors
  
  
  stopping_tolerance=1e-2,      
  stopping_rounds=2, #This and the previous two metrics tell h2o to stop when the average error does not improve beyond 
                     #the stopping_tolerance threshold for stopping_rounds number of rounds
  
  
  activation=c("Rectifier"), #This sets up the type of activation function to be used in hidden layers
  
  epochs=c(50),
  
  hyper_params=hyper_params,#This tells h2o to try each model implied by the parameters specified earlier in hyper_params list
  
  overwrite_with_best_model=TRUE,
  export_weights_and_biases=TRUE,
  
  reproducible=TRUE,
  seed=123
  
)

if (problem.type=="C"){
grid.summary <- h2o.getGrid(id,sort_by="err",decreasing=FALSE)} else {
  grid.summary <- h2o.getGrid(id,sort_by="mse",decreasing=FALSE)
}

View(grid.summary@summary_table)

#Getting the best model

grid.summary@summary_table[1,]
best_model <- h2o.getModel(grid.summary@model_ids[[1]])
best_model


#Get the weights and biases for the best model

################################ATTENTION########################################

#################################################################################
################ONLY TO BE USED FOR SMALL NETWORKS###############################

# num.hid.lay.best=length(best_model@allparameters$hidden)

# for (i in 1:(num.hid.lay.best+1)){

# print(paste("Weights between layer", i, "and", i+1, sep=" "))
# print(h2o.weights(best_model, matrix_id = i))

# print(paste("Biases for layer", i+1, sep=" "))
# print(h2o.biases(best_model, vector_id = i))}

#################################################################################
#################################################################################


#Plotting LIME Plots Below

#Below provide the row number of the testing set which you would like LIME to explain
row.to.explain=2

set.seed(100)
lime_to_explain <-mydata.test[row.to.explain,predictors] #Explaining the row stored in 'row.to.explain'
explainer <- suppressWarnings(lime(mydata.train, model = best_model)) #specifying that the training model will be used for explanations


if (problem.type=="R"){
  explanation <- explain(
    lime_to_explain, #the explanations will be given for these observations
    explainer, 
    n_features=5, #the top 5 features based on forward selection will be used in explanations
    feature_select = "forward_selection", #see above
    
    dist_fun = "euclidean",
    
    kernel_width = 0.5,
    
    
    n_permutations = 500 #for each obs in "lime_to_explain" there will be 5,00 permutations created
    #based on the data contained in "explainer", i.e. based on the variables
    #of training set. In other words, for each test set observation, 5K obs
    #are created using the distributions of the training data columns. Those then
    #are used to fit the local model in the vicinity of the test set obs in question.
  )
  
} else {
  
  explanation <- explain(
    lime_to_explain, #the explanations will be given for these observations
    explainer,
    n_labels = length(unique(mydata.test$myresponse)),
    n_features=5, #the top 5 features based on forward selection will be used in explanations
    feature_select = "forward_selection", #see above
    
    dist_fun = "euclidean",
    
    kernel_width = 0.5,
    
    
    n_permutations = 500 #for each obs in "lime_to_explain" there will be 5,00 permutations created
    #based on the data contained in "explainer", i.e. based on the variables
    #of training set. In other words, for each test set observation, 5K obs
    #are created using the distributions of the training data columns. Those then
    #are used to fit the local model in the vicinity of the test set obs in question.
  )
  
  
}  

plot_features(explanation)


#Predictive accuracy for the best model (evaluating on the test set)
#Note that for classification the class with the highest probability gets assigned as the prediction

predictions=h2o.predict(best_model, mydata.test.for.h2o)
predictions=as.data.frame(predictions)

if (problem.type=="C"){
classes=colnames(predictions)[2:ncol(predictions)]
predictions$tt=apply(predictions[,2:ncol(predictions)],1,which.max)
f<-function(i) classes[i]
predictions$predict=sapply(predictions$tt,f)

mydata.test.with.pred=cbind(mydata.test, predictions)
if (substr(mydata.test.with.pred$myresponse[1],1,1)!="p" & sum(substr(mydata.test.with.pred$predict,1,1)=="p")==nrow(mydata.test.with.pred)){
  mydata.test.with.pred$predict=gsub("p","",mydata.test.with.pred$predict)}

CrossTable(mydata.test.with.pred$myresponse,mydata.test.with.pred$predict,prop.chisq=F,prop.t=F)} else

{
  
  mydata.test.with.pred=cbind(mydata.test, predictions)
  
  rmse=sqrt(mean((mydata.test.with.pred$myresponse-mydata.test.with.pred$predict)^2))
  mape=100*mean(abs(mydata.test.with.pred$myresponse-mydata.test.with.pred$predict)/mydata.test.with.pred$myresponse)
  
  print(paste("RMSE: ", round(rmse,2)))
  print(paste("MAPE: ", round(mape,2)))
  
}
  



