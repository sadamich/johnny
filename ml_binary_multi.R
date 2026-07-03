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
y2<- DUMJCAT2[GENDER==1]
y3<- DUMJCAT3[GENDER==1]
const<- rep(1, 258)
p<- exp(beta1*const+beta2*EDUC+beta3*MINORITY)/sum(exp(beta1*const+beta2*EDUC+beta3*MINORITY))


l<- function(beta){
beta2_1<- beta[1]
beta2_2<- beta[2]
beta2_3<- beta[3]
beta3_1<- beta[4]
beta3_2<- beta[5]
beta3_3<- beta[6]
sum(y2*(beta2_1*const+beta2_2*EDUC+beta2_3*MINORITY)
   +y3*(beta3_1*const+beta3_2*EDUC+beta3_3*MINORITY)
- sum(log(1+ sum(exp(beta2_1*const+beta2_2*EDUC+beta2_3*MINORITY
                 +beta3_1*const+beta3_2*EDUC+beta3_3*MINORITY)))))
}
m<- maxLik(l, start= c(0,0,0,0,0,0))
summary(m)