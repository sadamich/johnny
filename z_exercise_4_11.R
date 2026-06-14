### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 11 (p.270)
set.seed(27)
e<- rt(50,3)
plot(e)
hist(e)
### Problem (a) 
y<- 1/2+e
plot(y)
hist(y)
summary(y)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-2.8724 -0.2124  0.3942  0.2431  0.8519  3.0791 

### Problem (b)
eq<- lm(y ~ 1)
summary(eq)
### OLS: Call:lm(formula = y ~ 1)
Residuals:
    Min      1Q  Median      3Q     Max 
-3.1155 -0.4555  0.1511  0.6088  2.8360 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   0.2431     0.1742   1.396    0.169
2*(1-pt(1.396,49))
[1] 0.1690077

Residual standard error: 1.232 on 49 degrees of freedom
### H0 (b1 =0) is not rejected.
y1<- y^2 - mean(y)^2
sd(y1)
[1] 2.138192  (sd of the sample mean)

### Problem (c)                                                            ###
library(gmm)
### Compare with the following example of the normal distribution          ###
### The moment conditions of the normal distribution                       ###
g1 <- function(tet,y)
{
m1 <- (tet[1]-y)
m2 <- (tet[2]^2- (y- tet[1])^2)
m3 <- y^3-tet[1]*(tet[1]^2+3*tet[2]^2)
f <- cbind(m1,m2,m3)
return(f)
}
Dg <- function(tet,y)
{
G <- matrix(c( 1,
2*(-tet[1]+mean(y)),-3*tet[1]^2-3*tet[2]^2,0,
2*tet[2],-6*tet[1]*tet[2]),
nrow=3,ncol=2)
return(G)
}
print(res<-gmm(g1,y,c(mu = 0, sig= 0), grad= Dg))
Method
 twoStep 
Objective function value:  0.05848714 
     mu      sig  
0.26737  1.16453  
Convergence code =  0 
(0.26737 - 0)/1.16453
[1] 0.2295948  ( t value) 
2*(1-pt(0.2295948,49))
[1] 0.8193624        (P value) 

0.2431 (Ols mu)          0.26737    (GMM Normal distribution)
0.1742 (Ols sd)          1.16453    (GMM Normal distribution)
1.396  (Ols tvalue)      1.16453    (GMM Normal distribution)
0.169  (Ols Pvalue)      0.8193624  (GMM Normal distribution)
### The theory of the relation between OLS and GMM                         ###
sqrt(49/50)*0.1742 
[1] 0.1724492  (the corrected GMM standard error as the second moment)
### Mu(GMM) = 0.2431 (Ols mu) as the first moment                          ###

### Problem (d)   Cauchy distribution                                      ###
library(maxLik)
f_cauchy<- function(theta){
mu<- theta[1]
sigma<- theta[2]
N<- 50
 -N*log(pi) -N*log(1+(y - mu)^2)
}
m<- maxLik(f_cauchy,start=c(1,1))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 3 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -4433.067 
2  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)
[1,]   0.4049        Inf       0       1
[2,]   1.0000        Inf       0       1
### t value = 0 and P value = 1. H0 is not rejected, because of s.e (Inf).  ###

### Problem (e)    t3 distribution
f_t3<- function(theta){
mu<- theta[1]
sigma<- theta[2]
N<- 50
-2*sum(log(1+1/3*(y-mu)^2))
}
m2<- maxLik(f_t3,start=c(1,1))
summary(m2)
Maximum Likelihood estimation
Newton-Raphson maximisation, 3 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -31.30702 
2  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)
[1,]    0.358        Inf       0       1
[2,]    1.000        Inf       0       1
### t value = 0 and P value = 1. H0 is not rejected, because of s.e (Inf).  ###

### Problem (f) GMM is the best method under the condition of unknown GDP   ###

