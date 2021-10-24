
####################################################################################################################

####################################################################################################################

####################################################################################################################

#############################This program was created by Davit Khachatryan##########################################
######©2016-2017 by Davit Khachatryan.  All rights reserved. No part of this document may  be reproduced or#########
######transmitted in any form or by any means, electronic, mechanical, photocopying, recording or otherwise#########
############################without prior written permission of Davit Khachatryan###################################

####################################################################################################################

####################################################################################################################

#####################################################################################################################


#########################################################################################
#########################################################################################
###################################ATTENTION#############################################
########THIS MACRO IS INTENDED FOR CLASSIFICATION AND REGRESSION TREES (CART)############
#########################################################################################
#########################################################################################

library(tree)
library(gmodels)


#############################################################################################
##############################UPDATE THE SECTION BELOW#######################################
#############################################################################################

#START OF SETUP

  
  #Enter "R" for a regression tree and "C" for a classificaiton tree below.

  tree_type="C"

  #Enter the minimum number of items in each leaf
 
  min_leaf_size=30

  #Enter the minimum deviance for a node to be considered for a further split

  min_deviance=0.025

#END OF SETUP


#START OF DATA IMPORT

#update the path below to point to the directory and name of your data in *.csv format  

  mydata=read.csv("C:/Users/dkhachatryan/Documents/Teaching/Fall 2019/QTM3635/Data/Spam_Data_In_Full.csv")

#END OF DATA IMPORT

#START OF VARIABLE REDEFINITION

  mydata$myresponse=mydata$Status #Substitute "Status" with the name of your response variable
  mydata$Status=NULL #Substitute "Status" with the name of your response variable

  str(mydata)

#END OF VARIABLE REDEFINITION

#In the following statements substitute the names after "$" sign with the names of prdictors
#in your data that are categorical but are read into R in a different format. If there are no such 
#variables in your data, then ignore.
  
#START OF PREDICTOR TRANSFORMATION
 
  #mydata$xyz=as.factor(mydata$xyz)
  #mydata$xyz=as.factor(mydata$xyz)
  #mydata$xyz=as.factor(mydata$xyz)
  
  #add statements similar to above as needed
  
#END OF PREDICTOR TRANSFORMATION
 
  
#START OF RESPONSE TRANSFORMATION
  
  #Remember that compared to a Regression Tree, when growing a Classificaiton Tree 
  #the response needs to be a factor.
  #Un-comment the line below only if the response needs to be converted to categorical(factor)
  
  mydata$myresponse=as.factor(mydata$myresponse)

#END OF RESPONSE TRANSFORMATION

#The statements below remove all the variables that will not be passed to the tree algorithm
#as predictors. If no such redundant variables exist in your dataset, then the statements
#in the "REDUNDANT VARIABLE REMOVAL" section should be deleted.

#START OF REDUNDANT VARIABLE REMOVAL

  #mydata$xyz=NULL #Substitute "xyz" with the name of the variable in your data that 
                      #will not be passed to the tree algorithm. Add as many statements similar 
                     #to this as needed.

#END OF REDUNDANT VARIABLE REMOVAL

#############################################################################################
#####################################ATTENTION###############################################
#############################################################################################

#######################IF THE ABOVE MODIFICATIONS ARE MADE CORRECTLY,########################
####AT THIS POINT "MYDATA" DATA FRAME SHOULD CONTAIN ONLY THE PREDICTORS AND THE OUTCOME.#### 
####IN CASE IT CONTAINS ANYTHING MORE OR LESS, THE CODE BELOW WILL NOT FUNCTION PROPERLY.####
#############################################################################################

str(mydata) #make sure the structure of your data reflects all the modifications made above

#############################################################################################
##############################DO NOT MODIFY THE CODE BELOW###################################
#############################################################################################


#START DATA BREAKDOWN FOR HOLDOUT METHOD

#Start finding the categorical predictors

numpredictors=dim(mydata)[2]-1

numfac=0

for (i in 1:numpredictors) {
  if ((is.factor(mydata[,i]))){
    numfac=numfac+1} 
}

#End finding the number of categorical predictors 

nobs=dim(mydata)[1]



if (tree_type=="R") {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Regression Tree
  
  train_size=floor(0.8*nobs)
  test_size=nobs-train_size

} else {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Classification Tree
  
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
    
    
    if (tree_type=="C") {
      
    train_index = c()
    
    for(i in 1:numb.class){
      index_temp = which(mydata$myresponse==class.names[i])
      train_index_temp = sample(index_temp, length.vector[i], replace = F)
      train_index = c(train_index, train_index_temp)
    }} else {
      train_index=sample(nobs,train_size, replace=F)
    }
    
    mydata_train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
    mydata_test=mydata[-train_index,]#everything not in the training set should go into testing set
    
    right_fac=0 #denotes the number of factors with "right" distributions (i.e. - the unique levels match across mydata, test, and train data sets)
    
    
    for (i in 1:numpredictors) {
      if (is.factor(mydata_train[,i])) {
        if (sum(as.vector(unique(mydata_test[,i])) %in% as.vector(unique(mydata_train[,i])))==length(unique(mydata_test[,i])))
          right_fac=right_fac+1
      }
    }
    
    if (right_fac==numfac) (resample=0) else (resample=1)
    
  }
  
dim(mydata_test) #confirms that testing data has only 20% of observations
dim(mydata_train) #confirms that training data has 80% of observations


#END DATA BREAKDOWN FOR HOLDOUT METHOD


#Start growing the reference tree
full_tree=tree(myresponse ~ .,split="deviance",mindev=min_deviance, mincut=min_leaf_size,data=mydata_train)
#End growing the reference tree


#START 10-FOLD CROSS-VALIDATION TO FIND THE SIZE THAT THE FULL TREE WILL BE REDUCED TO

b_list=rep(1,100)

for (i in 1:100){

  set.seed(i)
  cv_tree=cv.tree(full_tree,K=10)
  cv_tree$size
  cv_tree$dev
  bestsize=min(cv_tree$size[cv_tree$dev==min(cv_tree$dev)])
  b_list[i]=bestsize
  #plot(cv_tree, type="p")

}

mytable=as.data.frame(table(b_list))
mytable_s=mytable[order(mytable$Freq),]
final_tree_size=as.numeric(paste(mytable_s[dim(mytable_s)[1],1]))
#END K-FOLD CROSS-VALIDATION TO FIND THE SIZE THAT THE FULL TREE WILL BE REDUCED TO


#START REDUCING THE FULL TREE TO OPTIMAL SIZE AND PLOTTING

  bestcut=prune.tree(full_tree,best=final_tree_size)
  plot(bestcut, type=c("uniform"))
  text(bestcut, cex=0.6, digits = max(nchar(mydata$myresponse))+3)
  
  if (tree_type=="R"){
    print(bestcut, digits=max(nchar(mydata$myresponse))+3)} else
      print(bestcut)
  
  deviance(bestcut)

  
#END REDUCING THE FULL TREE TO OPTIMAL SIZE AND PLOTTING


#START PREDICTING THE RESPONSE IN THE TESTING SET (20 % SUBSET)

predicted=predict(bestcut,newdata=mydata_test, type="vector")

if (tree_type=="R") {
  id=as.numeric(names(predicted))
  temp_tbl=cbind(id,as.data.frame(predicted))
  mydata_test2=cbind(as.numeric(rownames(mydata_test)),mydata_test)
  colnames(mydata_test2)[1]="id"
  pred_table=merge(temp_tbl, mydata_test2, by.x="id", all.x=T)
  predicted=pred_table$predicted
  pred_table$predicted=NULL
  final_table=cbind(pred_table,predicted)
  final_table$id=NULL } else {

    predicted=as.data.frame(predicted)
    
    new.col = c()
    
    for(i in 1:(dim(predicted)[1])){
      
      find.max=which(predicted[i,]==max(predicted[i,]))
      
      #If there is a tie, assign a class randomly
      if (length(find.max)>1) {
        
        find.max=sample(find.max,1, replace=F)
        
      }
      
      new.col = c(new.col, names(predicted)[find.max])
    }
    
    
    predicted$predicted=new.col
    
    id=as.numeric(rownames(predicted))
    newdat=as.data.frame(id)
    newdat$predicted=predicted$predicted
    mydata_test2=cbind(as.numeric(rownames(mydata_test)),mydata_test)
    colnames(mydata_test2)[1]="id"
    final_table=merge(mydata_test2,newdat,by.x="id", all.x=T)}


#Measuring predictive accuracy below

if (tree_type=="R") {
  
  abs.diff=abs(final_table$predicted-final_table$myresponse)
  mape=100*mean(abs.diff/abs(final_table$myresponse))
  rmse=sqrt(mean(abs.diff^2))
  print(paste("MAPE for Testing Set Is:", 
              round(mape,2)))
  print(paste("RMSE for Testing Set Is:", 
              round(rmse,2)))} else {
                print("Confusion Matrix Is:")
                CrossTable(final_table$myresponse,final_table$predicted,prop.chisq=F,prop.t=F) }

#END PREDICTING THE RESPONSE IN THE TESTING SET (20 % SUBSET)

#############################################################################################
##############################THIS IS THE END OF THE MACRO###################################
#############################################################################################


