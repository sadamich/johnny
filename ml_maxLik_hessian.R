library(maxLik)
# log-likelihood for normal density
# a[1] - mean
# a[2] - standard deviation
ll <- function(a) sum(-log(a[2]) - (x - a[1])^2/(2*a[2]^2))
x <- rnorm(100) # sample from standard normal
ml <- maxLik(ll, start=c(1,1))
# ignore eventual warnings "NaNs produced in: log(x)"
summary(ml) # result should be close to c(0,1)
Maximum Likelihood estimation
Newton-Raphson maximisation, 7 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -48.35182 
2  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,] -0.10873    0.09837  -1.105   0.269    
[2,]  0.98365    0.06955  14.142  <2e-16 ***
hessian(ml) # How the Hessian looks like
          [,1]     [,2]
[1,] -103.3413    0.000
[2,]    0.0000 -206.704
sqrt(-solve(hessian(ml))) # Note: standard deviations are on the diagonal
           [,1]       [,2]
[1,] 0.09837007 0.00000000
[2,] 0.00000000 0.06955455
# Now run the same example while fixing a[2] = 1
mlf <- maxLik(ll, start=c(1,1), activePar=c(TRUE, FALSE))
summary(mlf) # first parameter close to 0, the second exactly 1.0
Maximum Likelihood estimation
Newton-Raphson maximisation, 2 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -48.37869 
1  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)
[1,]  -0.1087     0.1000  -1.087   0.277
[2,]   1.0000     0.0000      NA      NA
hessian(mlf)
          [,1] [,2]
[1,] -100.0018   NA
[2,]        NA   NA

# Note that now NA-s are in place of passive
# parameters.
# now invert only the free parameter part of the Hessian
sqrt(-solve(hessian(mlf)[activePar(mlf), activePar(mlf)]))
           [,1]
[1,] 0.09999911
# gives the standard deviation for the mean
