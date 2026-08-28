### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Comparison of methods (p. 41)
set.seed(27)
x<- rnorm(30,0,1)
e<- rnorm(30,0,1)
y<- x+e
### The method of moments
summary(y)
  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-3.38996 -1.24210  0.05074 -0.04323  1.32894  2.99192
hist(y)
sd(y)
[1] 1.73457
### OLS 
eq<- lm(y~x)
summary(eq)
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.1555     0.2067   0.752    0.458    
x             1.1596     0.1797   6.453 5.46e-07 ***
### ML 
library(maxLik)
f<- function(beta){
beta1<- beta[1]
beta2<- beta[2]
sigma<- beta[3]
mu<- beta1+beta2*x
n<- 30

-0.5*n*log(2*pi)- 0.5*n*log(sigma^2)-1/(2*sigma^2)*sum((y-mu)^2)
}
m<- maxLik(f, start = c(0,0,1))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 7 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -44.91598 
3  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]   0.1555     0.1997   0.779    0.436    
[2,]   1.1596     0.1736   6.679 2.41e-11 ***
[3,]   1.0814     0.1396   7.747 9.44e-15 ***

### Example 1 7 (p.39)
xm101<- read.csv("xm101.csv", header=TRUE)
attach(xm101)
str(xm101)
### The method of moments
summary(FGPA)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.500   2.485   2.773   2.793   3.116   3.971 
### The method of OLS
e<- rnorm(609,0,1)
y<- FGPA+e
eq1<- lm(y~FGPA)
summary(eq1)
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.01491    0.25920   0.058    0.954    
FGPA         0.99342    0.09158  10.848   <2e-16 ***
fit<- fitted(eq1)
summary(fit)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.505   2.484   2.770   2.789   3.110   3.960 
hist(fit)
### The method of ML
f1<- function(beta){
beta1<- beta[1]
beta2<- beta[2]
sigma<- beta[3]
mu<- beta1+beta2*FGPA
n<- 609

-0.5*n*log(2*pi)- 0.5*n*log(sigma^2)-1/(2*sigma^2)*sum((y-mu)^2)
}
m1<- maxLik(f1, start = c(0,0,1))
summary(m1)
Maximum Likelihood estimation
Newton-Raphson maximisation, 8 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -886.5679 
3  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,]  0.01491    0.25979   0.057   0.954    
[2,]  0.99342    0.09178  10.824  <2e-16 ***
[3,]  1.03752    0.02973  34.902  <2e-16 ***
