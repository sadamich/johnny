### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 15 (p. 529)   Microeconomics

xr615<- read.csv("xr615.csv", header=TRUE)
str(xr615)
attach(xr615)
### Problem (a) the ordered Logit model 
str(LEVELMATH)
y<- LEVELMATH
head(y, 300)
sat<- SATMATH/100
library(maxLik)
l<- function(beta){
beta1<- beta[1]
beta2<- beta[2]
beta3<- beta[3]
beta4<- beta[4]
beta5<- beta[5]
beta6<- beta[6]
beta7<- beta[7]
beta8<- beta[8]
beta9<- beta[9]
y*beta1*sat+y*beta2*FEMALE+y*beta3*MAJORESH+y*beta4*MAJORNAT
+y*beta5*ADVMATH1+y*beta6*ADVMATH2+y*beta7*ADVMATH3+y*beta8*PHYSICS
+y*beta9*CHEMISTRY

}
m<- maxLik(l, start= c(1,0,1,0,0,0,0,0,1))
summary(m)???
### Problem (b) the comparison of the ordered logit with the probit model

### Problem (c) the MNL mode




detach(xr615)