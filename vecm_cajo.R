### Compare with the Example 7 27 Interest and bond rates p.674            ###
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
library(urca)

x<- cbind(AAA, US3MTBIL)

x<-ca.jo(x, type = "trace", ecdet = "trend", K = 2,
spec="longrun")

ca.jo(x, type = "eigen", ecdet = "none", K = 3,
spec= "longrun")


cajolst(x, trend = TRUE, K = 2, season = NULL)


plotres(x)