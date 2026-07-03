### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 6 4 (p.470)                                                    ###
xm604<- read.csv("xm604.csv", header = TRUE)
attach(xm604)
str(xm604)
library(maxLik)
y<- JOBCAT[GENDER==1]
const<- rep(1, 258)
p<- exp(beta1*const+beta2*EDUC+beta3*MINORITY)/sum(exp(beta1*const+beta2*EDUC+beta3*MINORITY))


l<- function(beta){
beta1<- beta[1]
beta2<- beta[2]
beta3<- beta[3]
beta1<- 0
sum(y*((beta1*const)+(beta2*EDUC)+(beta3*MINORITY))- sum(log(1+ sum(exp(beta2*EDUC+beta3*MINORITY)))))
}
m<- maxLik(l, start= c(0, 1, 1))
summary(m)