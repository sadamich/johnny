### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)

leveragePlots(lm(LOGSAL~EDUC+LOGSALBEGIN+GENDER+MINORITY, data=xm301))