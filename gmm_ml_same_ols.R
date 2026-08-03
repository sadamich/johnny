### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
###

set.seed(43)
x<- rnorm(30, 0, 1)
e<- rnorm(30, 0, 1)
y<- 2*x+e

eq<- lm(y~x)
summary(eq)
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
### GMM = OLS                                                              ###


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

