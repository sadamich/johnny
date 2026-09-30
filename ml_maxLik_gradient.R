library(maxLik)
# ML estimation of exponential duration model:
t <- rexp(10, 2)
loglik <- function(theta) log(theta) - theta*t

## Estimate with numeric gradient and hessian
a <- maxLik(loglik, start=1 )
summary(a)
Maximum Likelihood estimation
Newton-Raphson maximisation, 4 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -6.774124 
1  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)   
[1,]   1.3807     0.4367   3.161 0.00157 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 
gradient(a)
[1] -1.31839e-08
hessian(a)
          [,1]
[1,] -5.242917

# Extract the gradients evaluated at each observation
library( sandwich )
z<- estfun( a )
 estfun( a )
             [,1]
 [1,]  0.27418114
 [2,] -1.08302996
 [3,]  0.07060283
 [4,]  0.46284226
 [5,]  0.52529927
 [6,]  0.15559161
 [7,]  0.24279291
 [8,]  0.33060279
 [9,] -1.62372278
[10,]  0.64483992
hist(z, freq=FALSE)
## Estimate with analytic gradient.
## Note: it returns a vector
gradlik <- function(theta) 1/theta - t
b <- maxLik(loglik, gradlik, start=1)
summary(b)
Maximum Likelihood estimation
Newton-Raphson maximisation, 4 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -6.774124 
1  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)   
[1,]   1.3807     0.4366   3.162 0.00157 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 
gradient(a)
estfun( b )