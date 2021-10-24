# session 4 adbudget.
#pre work
install.packages('GGally',dependencies = TRUE)
library('fpp2')
library('GGally')
rm(list=ls())
gc()
cat('\f')
df=read.csv('adbudget.csv')
#load data
View(df) # no need to combine collumns
anyNA(df)
str(df)
df.ts=ts(df[,2:4],start = c(1981,1),end =c(2005,4),frequency = 4)
# all 3 collumns in 1 object
df.portion=window(df.ts,start=c(1990,1),end=c(2000,4))
# a portion from the ts object from 1990 to 2000
library('fpp2')
autoplot(df.ts) + ggtitle('sales vs. adbudget vs  gpd')+ xlab('year')+ylab('')

autoplot(df.ts[,'Sales']) + ggtitle('sales vs. adbudget vs  gpd')+ xlab('year')+ylab('')

ggseasonplot(df.ts[,"Sales"], year.labels = TRUE,year.labels.left = TRUE) +ggtitle("seasonal plot of sales")+ylab('Sales)')

ggseasonplot(df.ts[,"Sales"], polar = TRUE, year.labels.left = TRUE) +ggtitle("polar plot of sales")+ylab('Sales')

#scatter plot matrix
ggpairs(as.data.frame(df.ts))
