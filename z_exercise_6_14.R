### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 14 (p. 529)   Bank wages

xr614<- read.csv("xr614.csv", header=TRUE)
str(xr614)
attach(xr614)
### Problem (a) MNL model
const<- rep(1, 474)
y2<- DUMJCAT2
y3<- DUMJCAT3
l<- function(beta){
beta2_1<- beta[1]
beta2_2<- beta[2]
beta2_3<- beta[3]
beta3_1<- beta[4]
beta3_2<- beta[5]
beta3_3<- beta[6]
y2*beta2_1*const+y2*beta2_2*EDUC+y2*beta2_3*MINORITY
   +y3*beta3_1*const+y3*beta3_2*EDUC+y3*beta3_3*MINORITY
 - log(1+exp(beta2_1*const+beta2_2*EDUC+beta2_3*MINORITY
                 +beta3_1*const+beta3_2*EDUC+beta3_3*MINORITY))
}
m<- maxLik(l, start= c(0,0,0,0,0,0))
summary(m)
### Problem (b) the coefficient = - Inf, describe a practical method

### Problem (c) the mixed model

### Problem (d) the comparison of b and c

### Problem (e) the diagnostic







detach(xr614)