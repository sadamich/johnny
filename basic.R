### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm101<- read.csv("xm101.csv", header=TRUE)
str(xm101)
attach(xm101)
hist(FGPA)
x<- FGPA
summary(x)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.500   2.485   2.773   2.793   3.116   3.971 
skew<- function(y,mean,n,sd){
result<- 1/n*(sum ((y - mean)^3))/sd^3
return(result)
}
skew(x,mean(x),609,sd(x))
[1] 0.1674154
kurt<-  function(y,mean,n,sd){
result<- (1/n*(sum ((y - mean)^4)))/sd^4
return(result)
}
kurt(x,mean(x),609,sd(x))
[1] 2.502284

library(REdaS)
densbox(x~1, main="FGPA")
mean(x)
[1] 2.792796
sd(x)
[1] 0.4602375

curve(dnorm(x, 2.792796,0.4602375),from = -3, to= 5)
dnorm(1, 2.792796,0.4602375)


