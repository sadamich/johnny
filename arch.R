### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
library(forecast)
xm702<- read.csv("xm702.csv", header =TRUE)
str(xm702)
attach(xm702)
acf(DJRET)
### Exhibit 7 25 b (p.632)                                                 ###
hist(DJRET)
summary(DJRET)


