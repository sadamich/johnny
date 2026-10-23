### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)

dfbetaPlots(lm(LOGSAL~EDUC+LOGSALBEGIN+GENDER+MINORITY, data=xm301))

dfbetasPlots(glm(LOGSAL ~ EDUC+LOGSALBEGIN+GENDER+MINORITY, 
  data=xm301, family=gaussian))