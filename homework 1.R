# Homework 1
#Xavier Torres.
#pre work
rm(list=ls())
gc()
cat('\f')
library('fpp2')
library('GGally')
#load the data and name the data
df=read.csv('C://Users/xtorres1/Desktop/R/Datasets/session 4/Restaurant Sales.csv')
# check data for na's
anyNA(df)
# check the structure of the data
str(df)

# in the data there are 3 variables that are able to be used for time serires, there are 5 varaibles in the data set.
#turn into ts object
df.ts=ts(df[,3:5],start = c(1,1),frequency = 6)
#start plotting
autoplot(df.ts)

# seasonal plots
ggseasonplot(df.ts[,1], year.labels=TRUE, year.labels.left=TRUE) + ylab("Sales") +xlab('days of the week')+
ggtitle("Seasonal plot: lunch sales") 
ggseasonplot(df.ts[,2], year.labels=TRUE, year.labels.left=TRUE) + ylab("Sales") +xlab('days of the week')+
ggtitle("Seasonal plot: dinner sales") 
ggseasonplot(df.ts[,3], year.labels=TRUE, year.labels.left=TRUE) + ylab("Sales") + xlab('days of the week')+ 
ggtitle("Seasonal plot: delivery sales")

# seasonal polar plots
ggseasonplot(df.ts[,1],polar = TRUE)+ggtitle('Lunch Sales')+ylab('Sales')+xlab('days of the week')+scale_color_discrete(name='weeks') 
ggseasonplot(df.ts[,2],polar = TRUE)+ggtitle('Dinner Sales')+ylab('Sales')+xlab('days of the week')+scale_color_discrete(name='weeks') 
ggseasonplot(df.ts[,3],polar = TRUE)+ggtitle('Delivery Sales')+ylab('Sales')+xlab('days of the week')+scale_color_discrete(name='weeks') 

#subseries plots
ggsubseriesplot(df.ts[,1]) + ylab("Sales")+ggtitle("Lunch sales")+xlab('days of the week')
ggsubseriesplot(df.ts[,2]) + ylab("Sales")+xlab('days of the week')+ggtitle("Dinner sales")
ggsubseriesplot(df.ts[,3]) + ylab("Sales")+xlab('days of the week')+ggtitle("delivery sales")

#scatterplot matrix
ggpairs(as.data.frame(df.ts))

#end of homework 1
