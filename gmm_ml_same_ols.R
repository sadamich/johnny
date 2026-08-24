### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
###

set.seed(43)
x<- rnorm(30, 0, 1)
e<- rnorm(30, 0, 1)
y<- 2*x+e
plot(x)
plot(y)
hist(x)
hist(y)
eq<- lm(y~x)
summary(eq)
res<- resid(eq)
plot(x,res)
hist(res)
Residuals:
     Min       1Q   Median       3Q      Max 
-2.17000 -0.29924 -0.05075  0.72563  1.58898 
skew<- function(y,mean,n,sd){
result<- 1/n*(sum ((y - mean)^3))/sd^3
return(result)
}
skew(res,mean(res),30,sd(res))
[1] -0.2155924
kurt<-  function(y,mean,n,sd){
result<- 1/n*(sum ((y - mean)^4))/sd^4
return(result)
}
kurt(res,mean(res),30,sd(res))
[1] 2.54831

### OLS Call: lm(formula = y ~ x)
Residuals:
     Min       1Q   Median       3Q      Max 
-2.17000 -0.29924 -0.05075  0.72563  1.58898 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.1659     0.1704   0.973    0.339    
x             2.2838     0.1770  12.901 2.65e-13 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.9244 on 28 degrees of freedom
Multiple R-squared:  0.856,     Adjusted R-squared:  0.8509 
F-statistic: 166.4 on 1 and 28 DF,  p-value: 2.649e-13

library(gmm)
eq_gmm<- gmm(y~x, x=x)
summary(eq_gmm)
### GMM : Call: gmm(g = y ~ x, x = x)
Method:  twoStep 
Kernel:  Quadratic Spectral
Coefficients:
             Estimate    Std. Error  t value     Pr(>|t|)  
(Intercept)  1.6590e-01  2.0641e-01  8.0374e-01  4.2155e-01
x            2.2838e+00  1.2264e-01  1.8623e+01  2.0971e-77
J-Test: degrees of freedom is 0 
                J-test                P-value             
Test E(g)=0:    7.85207119265002e-31  ******* 
library(sandwich)
sandwich(eq_gmm)
            [,1]        [,2]
[1,] 0.027642036 0.005159768
[2,] 0.005159768 0.017977287
bread(eq_gmm)
          [,1]      [,2]
[1,] 1.2780984 0.1072541
[2,] 0.1072541 0.4511909
meat(eq_gmm)
            [,1]        [,2]
[1,]  0.50104347 -0.07205672
[2,] -0.07205672  2.65520576




### GMM = OLS                                                              ###

### ML estimate                                                            ###
ml<- function(theta){
beta1 <- theta[1]
beta2 <- theta[2]
sigma <- theta[3]
mu<- beta1 + beta2*x
u <- y - mu
N<- 30
 -N/2*log(2*pi) - N/2*log(sigma^2) - 1/(2*sigma^2)*sum(u^2)
}
library(maxLik)
m<- maxLik(ml, start = c(0,0,1))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 8 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -39.17407 
3  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,]   0.1659     0.1646   1.008   0.314    
[2,]   2.2838     0.1710  13.355 < 2e-16 ***
[3,]   0.8930     0.1153   7.746 9.5e-15 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
### GMM = ML = OLS                                                         ###


?????????
u<- y - x
g <- function(u,x)
        {      
        m1 <- u
        m2 <- x*u
        f <- cbind(m1, m2)
        return(f)
        }
# Implementing the jacobian
Dg <- function(x,u)
        {
        jacobian <- matrix(c(u^2, u^2*x, u^2*x,u^2*x^2), nrow=2,ncol=2)
        return(jacobian)
        }
# Now we want to estimate the two parameters using the GMM.
gmm(g, x, c(0, 1), grad = Dg)

