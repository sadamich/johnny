### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

library(tsDyn)
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
detach(xm722)
x<- ts(AAA, freq=12, start=1948)
plot(x, main="Time series", ylab="AAA")
grid<-selectSETAR(AAA, m=1, thDelay=0, trim=0.15, criterion="SSR")

var_data<- data.frame(AAA[25:624],US3MTBIL[25:624])
eq<- VECM(var_data,lag=2, estim="ML")




### The one threshold case
data(lynx)
str(lynx)
Time-Series [1:114] from 1821 to 1934: 269 321 585 871 147
x<- ts(lynx, freq=1, start=1821)
plot(x, main="Time series", ylab="lynx")
grid<-selectSETAR(lynx, m=1, thDelay=0, trim=0.15, criterion="SSR")