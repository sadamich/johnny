### The maxLik function ###
library(maxLik)
### Source:https://cran.r-project.org/web/packages/maxLik/vignettes/using-maxlik.pdf ###
x<- rnorm(100)
loglik<- function(theta){
mu <- theta[1]
sigma <- theta[2]
sum(dnorm(x, mean=mu, sd = sigma, log=TRUE))
}
m<- maxLik(loglik, start = c(mu=1, sigma=2))
summary(m)

### OLS and ML Estimate ###
library(maxLik)
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press, p.724                 ###                        ###
fgpa<- c(1.8, 2.4, 2.9, 3.0,3.5)
satm<- c(4,6, 6, 7, 8)
satv<- c(4, 5, 7, 6, 7)
y<- fgpa
con<- c(1, 1, 1, 1, 1)
X<- cbind(con, satm, satv)
X
negSSE<- function(beta){
e<- y - X%*% beta
    -crossprod(e)
}
### ML Estimation ###
m<- maxLik(negSSE, start = c(0,0,0))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 2 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -0.01476636 
3  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)
[1,]  -0.1832     1.6451  -0.111   0.911
[2,]   0.2654     0.3986   0.666   0.505
[3,]   0.2168     0.4534   0.478   0.633
### OLS Estimation ###
eq01<- lm(y~X-1)
summary(eq01)
Call:lm(formula = y ~ X - 1)
Residuals:
       1        2        3        4        5 
 0.05421 -0.09346 -0.02710  0.02430  0.04206 

Coefficients:
      Estimate Std. Error t value Pr(>|t|)  
Xcon  -0.18318    0.19988  -0.916   0.4562  
Xsatm  0.26542    0.04844   5.480   0.0317 *
Xsatv  0.21682    0.05510   3.935   0.0589 .
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.08593 on 2 degrees of freedom
Multiple R-squared:  0.9996,    Adjusted R-squared:  0.999 
F-statistic:  1745 on 3 and 2 DF,  p-value: 0.0005729

