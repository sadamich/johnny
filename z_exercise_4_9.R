### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 9 
### Problem (a)
x<- runif(100, 0, 20)
plot(x)
hist(x)
e<- rnorm(100, 0, 0.01)
y<- 2+sqrt(x)+e
plot(y)
hist(y)
const<- rep(1,100)
X<- cbind(const, x)

### Problem (b)
grad<- function(beta1,beta2,beta3){
g1<- 1
g2<- x^beta3
g3<- beta2*x^beta3*log(x)
G<- cbind(g1,g2,g3)
}
G<- grad(0,1,1)
head(G, 5)


### Problem (d)                                                            ###
G_0<- grad(0,1,0)
head(G_0,5)
     g1 g2          g3
[1,]  1  1 -0.09932345
[2,]  1  1  2.33476197
[3,]  1  1  2.38829509
[4,]  1  1  1.26166476
[5,]  1  1  1.55030841
### g1 = g2 =1. That means that the parameter is not idetified.            ###

### Problem (c)
f<- function(beta){
beta1<- beta[1]
beta2<- beta[2]
beta3<- beta[3]
g1<- 1
g2<- x^beta3
g3<- beta2*x^beta3*log(x)
X<- cbind(g1,g2,g3)
e<- y - X
-crossprod(e)
}
library(maxLik)
m<- maxBHHH(f,start= c(1,1,1)) 
summary(m)

library(gslnls)
eq_nls<- gsl_nls(fn= y ~ beta1 + beta2*x^(beta3)
, start = c(beta1= 0,beta2=1,beta3=1))
summary(eq_nls)
Nonlinear regression model
  model: y ~ beta1 + beta2 * x^(beta3)
 beta1  beta2  beta3 
2.0002 0.9999 0.4997 
 residual sum-of-squares: 0.01045
Algorithm: multifit/levenberg-marquardt, (scaling: more, solver: qr)
Number of iterations to convergence: 11 
Achieved convergence tolerance: 7.841e-16
res_nls<- resid(eq_nls)

### Problem (e) LM test 
eq_lm<- lm(res_nls ~ sqrt(x)+ sqrt(x)*log(x))
summary(eq_lm)
100* 0.007244
[1] 0.7244
1-pchisq(0.7244,1)
[1] 0.3947044   (P value)
### H0 (beta3=1/2) is not rejected.                                       ###

library(maxLik)
negSSE<- function(beta){
beta1<- beta[1]
beta2<- beta[2]
beta3<- beta[3]
e<- y - (beta1+beta2*x^beta3)
- crossprod(e)
}

m<- maxLik(negSSE,start = c(0,1,1))
summary(m)