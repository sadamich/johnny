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

library(gslnls)
gsl_nls(fn= y ~ beta1 + beta2*x^(beta3), start = c(beta1= 0,beta2=1,beta3=0))
Nonlinear regression model
  model: y ~ beta1 + beta2 * x^(beta3)
 beta1  beta2  beta3 
1.9989 1.0031 0.4989 
 residual sum-of-squares: 0.009069

Algorithm: multifit/levenberg-marquardt, (scaling: more, solver: qr)

Number of iterations to convergence: 12 
Achieved convergence tolerance: 3.477e-12





library(maxLik)
negSSE<- function(beta){
beta1<- beta[1]
beta2<- beta[2]
beta3<- beta[3]
X<- c(1,x^beta3,beta2*x^(beta3)*log(x))
e<- y - X
- crossprod(e)
}

m<- maxLik(negSSE, start = c(0,1,0))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 13 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -0.009069476 
3  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]   1.9989     0.6727   2.972 0.002963 ** 
[2,]   1.0031     0.5478   1.831 0.067078 .  
[3,]   0.4989     0.1396   3.573 0.000352 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1








### Problem (d)

--------------------------------------------
Maximum Likelihood estimation
Newton-Raphson maximisation, 1 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: 0 
3  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)
[1,]        0        Inf       0       1
[2,]        1        Inf       0       1
[3,]        0        Inf       0       1
-------------------------------------

m_c<- maxLik(f, start = c(0,1,1,1),constraints=list(eqA=A, eqB=B) )
summary(m_c)
--------------------------------------------
Maximum Likelihood estimation
Newton-Raphson maximisation, 2 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -0.5 
4  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)
[1,]      0.0        Inf       0       1
[2,]      1.0        Inf       0       1
[3,]      0.5        Inf       0       1
[4,]      1.0        Inf       0       1

Warning: constrained likelihood estimation. Inference is probably wrong
Constrained optimization based on SUMT 
Return code: 1 
penalty close to zero 
1  outer iterations, barrier value 7.914374e-12 
--------------------------------------------
m_c<- maxLik(f, start = c(0,1,1,0),constraints=list(eqA=A, eqB=B) )
summary(m_c)


