### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)
https://cran.r-project.org/web/packages/car/refman/car.html#mmps

c1 <- lm(LOGSAL ~ EDUC, xm301)
mmps(c1)
c2 <- update(c1, ~ LOGSALBEGIN)
mmps(c2)
# include SD lines
p1 <- lm(LOGSAL ~ EDUC, xm301)
mmps(p1, sd=TRUE)
# condition on type:
mmps(p1, ~. | GENDER)

### cf) Chapter 6 
# logisitic regression example
# smoothers return warning messages.
# fit a separate smoother and color for each type of occupation.
m1 <- glm(JOBCAT ~ ., family=gaussian, data=xm301)
mmps(m1)

