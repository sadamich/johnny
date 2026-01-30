### Package maxLik ###
### Source: https://cran.r-project.org/web/packages/maxLik/refman/maxLik.html ###

library(maxLik)
set.seed(123)
x <- rnorm(50, 1, 2 )
## log likelihood function.
## Note: 'param' is a 2-vector c(mu, sd)
llf <- function(param) {
   mu <- param[1]
   sd <- param[2]
   llValue <- dnorm(x, mean=mu, sd=sd, log=TRUE)
   sum(llValue)
}
## Estimate it with mu=0, sd=1 as start values
ml <- maxLik(llf, start = c(mu=0, sigma=1) )
print(summary(ml))
coef(ml)
stdEr(ml)
AIC(ml)

## Create a 'maxControl' object:
maxControl(tol=1e-4, sann_tmax=7, printLevel=2)
## Optimize quadratic form t(D) %*% W %*% D with p.d. weight matrix,
## s.t. constraints sum(D) = 1
quadForm <- function(D) {
   return(-t(D) %*% W %*% D)
}
eps <- 0.1
W <- diag(3) + matrix(runif(9), 3, 3)*eps
D <- rep(1/3, 3)
                        # initial values
## create control object and use it for optimization
co <- maxControl(printLevel=2, qac="marquardt", marquardt_lambda0=1)
res <- maxNR(quadForm, start=D, control=co)
print(summary(res))
## Now perform the same with no trace information
co <- maxControl(co, printLevel=0)
res <- maxNR(quadForm, start=D, control=co) # no tracing information
print(summary(res))  # should be the same as above
maxControl(res) # shows the control structure


## a two-dimensional exponential hat
f <- function(a) exp(-a[1]^2 - a[2]^2)
## maximize wrt. both parameters 
free <- maxNR(f, start=1:2) 
summary(free)  # results should be close to (0,0)
activePar(free)
## keep the first parameter constant
cons <- maxNR(f, start=1:2, fixed=c(TRUE,FALSE))
summary(cons) # result should be around (1,0)
activePar(cons)


## ML estimation of exponential duration model:
t <- rexp(100, 2)
loglik <- function(theta) log(theta) - theta*t
## Estimate with numeric gradient and hessian
a <- maxLik(loglik, start=1 )
# Extract the "bread"
library( sandwich )
bread( a )
all.equal( bread( a ), vcov( a ) * nObs( a ) )

## compute MLE parameters of normal random sample
x <- rnorm(100)
loglik <- function(theta) {
   dnorm(x, mean=theta[1], sd=theta[2], log=TRUE)
}
m <- maxLik(loglik, start=c(mu=0, sd=1))
summary(m)
confint(m)
confint(m, "mu", level=0.1)


## ML estimation of exponential duration model:
t <- rexp(100, 2)
loglik <- function(theta) log(theta) - theta*t
gradlik <- function(theta) 1/theta - t
hesslik <- function(theta) -100/theta^2
## Estimate with analytic gradient and hessian
a <- maxLik(loglik, gradlik, hesslik, start=1)
## print log likelihood value
logLik( a )
## print log likelihood value of summary object
b <- summary( a )
logLik( b )

## Estimate the parameter of exponential distribution
t <- rexp(100, 2)
loglik <- function(theta) log(theta) - theta*t
gradlik <- function(theta) 1/theta - t
hesslik <- function(theta) -100/theta^2
## Estimate with numeric gradient and hessian
a <- maxLik(loglik, start=1, control=list(printLevel=2))
summary( a )
##
## Estimate with analytic gradient and hessian.
## require much smaller tolerance
## setting 'tol=0' or negative essentially disables this stopping criterion
a <- maxLik(loglik, gradlik, hesslik, start=1,
            control=list(tol=-1, reltol=1e-12, gradtol=1e-12))
summary( a )
##
## Next, we give an example with vector argument:
## fit normal distribution by estimating mean and standard deviation
## by maximum likelihood
##
loglik <- function(param) {
                           # param: vector of 2, c(mean, standard deviation)
   mu <- param[1]
   sigma <- param[2]
   ll <- -0.5*N*log(2*pi) - N*log(sigma) - sum(0.5*(x - mu)^2/sigma^2)
                           # can use dnorm(x, mu, sigma, log=TRUE) instead
   ll
}
x <- rnorm(100, 1, 2) # use mean=1, stdd=2
N <- length(x)
res <- maxLik(loglik, start=c(0,1)) # use 'wrong' start values
summary(res)
##
## Same example, but now with named parameters and a fixed value
##
resFix <- maxLik(loglik, start=c(mu=0, sigma=1), fixed="sigma")
summary(resFix)