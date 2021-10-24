#sesssion 4
#Pre work

#clear enviorment and previous 
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
###
# Murphybrothers.csv
install.packages('fpp2',dependencies = TRUE) # Install the packages
library('fpp2') # Load the packages
murphybro.df=read.csv('C:/Users/xtorres1/Desktop/R/Datasets/session 4/murphybrothers.csv')
View(murphybro.df)
anyNA(murphybro.df)
str(murphybro.df) #names of columns and datatypes
#multiples coluns, need to be handled

t(names(murphybro.df)) #show column n of each variable

#data slicing
murphybro.df[2,3] #2nd row, 3rd collumn (feb of 1993)
murphybro.df[2,] # 2nd row, all collumns (all data from feb of all years)
murphybro.df[0,3] # all data from 1993
murphybro.df[1:3,] #row 1 to 3, for all years
murphybro.df[,3:5] #all data from 1993 to 1995
murphybro.df[c(1,3,5),]# c for concatenate or combine those together
murphybro.df[,c(2,5)] # use c when rows are not together

str(murphybro.df)
MB.tsdf= unlist(murphybro.df[,2:5]) # combine columns 2 5 
View(MB.tsdf)

#defining a TS object

MB.ts=ts(MB.tsdf,start = c(1992,1),frequency =12)
#start from jan of 92, 12 months in each seasonal pattern
MB.ts=ts(MB.tsdf,start = c(1992,1),end = c(1995,12),frequency =12)
#good idea to use a stop as sometimes it will create false empty entries
MB.ts

#ts object slicing
MB.portion=window(MB.ts,start=c(1993,1),end=c(1994,3))
#####
#Visuals
#TS plot

autoplot(MB.ts) # TS plot
autoplot(MB.ts)+ggtitle('Murphy Brothers Monthly Sales')+ylab("Sales")+xlab('Years')

# more plots 
ggseasonplot(MB.ts,year.labels = TRUE,year.labels.left = TRUE)+ylab('Sales')+ggtitle('Seasonal Plot: Murphy Brothers Sales')
#polar seasonal plot
ggseasonplot(MB.ts,polar = TRUE)+ylab('Sales')+ggtitle('Polar seasonal plot')
# seasonal subseries
ggsubseriesplot(MB.ts)+ylab('sales')+ggtitle('seasonal subseries MB')
