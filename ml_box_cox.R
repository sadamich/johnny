### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 5 Bank wages (p.300)                                         ###

xm501<- read.csv("xm501.csv",header =TRUE)
str(xm501)
attach(xm501)
loglik <- function(theta) {
   beta<- theta[1]
   beta2<- theta[2]
   beta3<- theta[3]
   beta4<- theta[4]                        
   lambda<- theta[5]
   sigma <- theta[6]
   y_lam<- ((SALARY/10000)^lambda - 1)/lambda
   EDUC_lam <- (EDUC^lambda -1)/lambda
   GENDER_lam <- (GENDER^lambda -1)/lambda
   MINORITY_lam <- (MINORITY^lambda -1)/lambda
   mu<- (beta^lambda -1)/lambda + beta2*GENDER_lam + beta3*MINORITY_lam + 
         beta4*EDUC_lam
   ll <- -0.5*N*log(2*pi) - 0.5*N*log(sigma^2) 
          -(lambda -1)*sum(log(SALARY/10000))- sum(0.5*(y_lam - mu)^2/sigma^2)                           
   
}
N <- 474
library(maxLik)
m<- maxLik(loglik, start=c(0,0,0,0,0,0))
summary(m)
???