### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)
https://cran.r-project.org/web/packages/car/refman/car.html#mcPlots
m1 <- lm(LOGSAL ~ EDUC+LOGSALBEGIN+GENDER+MINORITY, data = xm301)
mcPlot(m1, "EDUC")
mcPlot(m1, "EDUC", overlaid=FALSE, ellipse=TRUE)

