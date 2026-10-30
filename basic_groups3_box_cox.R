### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)
https://cran.r-project.org/web/packages/car/refman/car.html#boxCox
### Compare with the panel 2 (p.300) 
with(xm301, boxCox(LOGSAL ~ EDUC+ LOGSALBEGIN + GENDER+MINORITY, data = xm301,
         lambda = seq(-0.25, 0.25, length = 10)))

https://cran.r-project.org/web/packages/car/refman/car.html#boxCoxVariable

eq<- lm(SALARY ~ EDUC+GENDER+MINORITY)
eq_bc<- update(eq, . ~ .+ boxCoxVariable(SALARY+1))
summary(eq_bc)