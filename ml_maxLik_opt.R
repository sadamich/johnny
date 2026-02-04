### Package maxLik ###
### Source: https://cran.r-project.org/web/packages/maxLik/refman/maxLik.html   ###
### Compare with 4.2.3 Non-linear optimization of Christiaan Heij, Paul de      ###
### Boer, Philip Hans Franses,Teun Kloek,Herman K. van Dijk (2004).Econometric  ###
### Methods with Applications in Business and Economics. Oxford University Press###                           
### https://global.oup.com/booksites/content/0199268010/                        ###
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
### Marquart Hessian correction ###
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

## A simple example with sin(x)' = cos(x)
f <- function(x) c(sin=sin(x))
Dsin <- compareDerivatives(f, cos, t0=c(angle=1))
### Example of normal log-likelihood.  Two-parameter function.###
x <- rnorm(100, 1, 2) # generate rnorm x
l <- function(b) sum(dnorm(x, mean=b[1], sd=b[2], log=TRUE))
gradl <- function(b) {
    c(mu=sum(x - b[1])/b[2]^2,
    sigma=sum((x - b[1])^2/b[2]^3 - 1/b[2]))
}
gradl. <- compareDerivatives(l, gradl, t0=c(mu=1,sigma=2))
## An example with f returning a vector, t0 = a scalar
trig <- function(x)c(sin=sin(x), cos=cos(x))
Dtrig <- function(x)c(sin=cos(x), cos=-sin(x))
Dtrig. <- compareDerivatives(trig, Dtrig, t0=1)

set.seed(0)
### generate a simple nearly multicollinear dataset  ###
x1 <- runif(100)
x2 <- runif(100)
x3 <- x1 + x2 + 0.000001*runif(100) # this is virtually equal to x1 + x2
x4 <- runif(100)
y <- x1 + x2 + x3 + x4 + rnorm(100)
m <- lm(y ~ -1 + x1 + x2 + x3 + x4)
print(summary(m)) # note the outlandish estimates and standard errors
                     # while R^2 is 0.88. This suggests multicollinearity
condiNumber(model.matrix(m))   # note the value 'explodes' at x3
### we may test the results further:
print(summary(lm(x3 ~ -1 + x1 + x2)))
### Note the extremely high t-values and R^2: x3 is (almost) completely ###
### explained by x1 and x2 (R^2 = 1)                                    ###

### Example with 'optim'                                                ###
fn <- function(x) (x[2]-2*x[1])^2
### note: true minimum is 0 on line 2*x[1] == x[2]                      ###
fullEst <- optim(par=c(1,1), method="BFGS", fn=fn)
fullEst$par
### par = c(0.6, 1.2) at minimum (not convex)                           ###
### Fix the last component to 4                                         ###
est4 <- optim(par=1, fn=fnSubset, method="BFGS", fnFull=fn, xFixed=4)
est4$par
### now there is a unique minimun x[1] = 2                              ###
### Fix the first component                                             ###
fnSubset(x=1, fnFull=fn, xFixed=c(a=4), xFull=c(a=1, b=2))
### After substitution:  xFull = c(a=4, b=1),                           ###
### so fn = (1 - 2*4)^2 = (-7)^2 = 49                                   ###
est4. <- optim(par=1, fn=fnSubset, method="BFGS",
               fnFull=fn, xFixed=c(a=4), 
               xFull=c(a=1, b=2))
est4.$par
### At optimum: xFull=c(a=4, b=8),                                      ###
### so fn = (8 - 2*4)^2 = 0                                             ###
### Example with 'maxLik'                                               ###
fn2max <- function(x) -(x[2]-2*x[1])^2
### -> need to have a maximum                                           ###
max4 <- maxLik(fnSubset, start=1, fnFull=fn2max, xFixed=4)
summary(max4)
### Similar result using fixed parameters in maxNR, called by maxLik    ###
max4. <- maxLik(fn2max, start=c(1, 4), fixed=2)
summary(max4.)

### ML estimation of exponential duration model:                        ###
t <- rexp(10, 2)
loglik <- function(theta) log(theta) - theta*t
### Estimate with numeric gradient and hessian                          ###
a <- maxLik(loglik, start=1 )
gradient(a)
### Extract the gradients evaluated at each observation                 ###
library( sandwich )
estfun( a )
### Estimate with analytic gradient.                                    ###
### Note: it returns a vector                                           ###
gradlik <- function(theta) 1/theta - t
b <- maxLik(loglik, gradlik, start=1)
gradient(a)
estfun( b )

### log-likelihood for normal density                                 ###
### a[1] - mean                                                       ###
### a[2] - standard deviation                                         ###
ll <- function(a) sum(-log(a[2]) - (x - a[1])^2/(2*a[2]^2))
x <- rnorm(100) # sample from standard normal
ml <- maxLik(ll, start=c(1,1))
### ignore eventual warnings "NaNs produced in: log(x)"               ###
summary(ml) # result should be close to c(0,1)
hessian(ml) # How the Hessian looks like
sqrt(-solve(hessian(ml))) #Note: standard deviations are on the diagonal#
### Now run the same example while fixing a[2] = 1                    ###
mlf <- maxLik(ll, start=c(1,1), activePar=c(TRUE, FALSE))
summary(mlf) # first parameter close to 0, the second exactly 1.0
hessian(mlf)
# Note that now NA-s are in place of passive
# parameters.
# now invert only the free parameter part of the Hessian
sqrt(-solve(hessian(mlf)[activePar(mlf), activePar(mlf)]))
# gives the standard deviation for the mean

### Maximum Likelihood estimation of Poissonian distribution          ###
n <- rpois(100, 3)
loglik <- function(l) n*log(l) - l - lfactorial(n)
### we use numeric gradient                                           ###
summary(maxBFGS(loglik, start=1))
### you would probably prefer mean(n) instead of that                 ###
### Note also that maxLik is better suited for Maximum Likelihood     ###
### Now an example of constrained optimization                        ###
f <- function(theta) {
  x <- theta[1]
  y <- theta[2]
  exp(-(x^2 + y^2))
  ## you may want to use exp(- theta %*% theta) instead
}
### use constraints: x + y >= 1                                       ###
A <- matrix(c(1, 1), 1, 2)
B <- -1
res <- maxNM(f, start=c(1,1), constraints=list(ineqA=A, ineqB=B),
control=list(printLevel=1))
print(summary(res))

### Fit exponential distribution by ML                                ###
t <- rexp(100, 2)  # create data with parameter 2
loglik <- function(theta) sum(log(theta) - theta*t)
### Note the log-likelihood and gradient are summed over observations ###
gradlik <- function(theta) sum(1/theta - t)
hesslik <- function(theta) -100/theta^2
### Estimate with finite-difference gradient and Hessian              ###
a <- maxNR(loglik, start=1, control=list(printLevel=2))
summary(a)
### You would probably prefer 1/mean(t) instead                       ###
### The same example with analytic gradient and Hessian               ###
a <- maxNR(loglik, gradlik, hesslik, start=1)
summary(a)
### BFGS estimation with finite-difference gradient                   ###
a <- maxBFGSR( loglik, start=1 )
summary(a)
### For the BHHH method we need likelihood values and gradients       ###
### of individual observations, not the sum of those                  ###
loglikInd <- function(theta) log(theta) - theta*t
gradlikInd <- function(theta) 1/theta - t
### Estimate with analytic gradient                                   ###
a <- maxBHHH(loglikInd, gradlikInd, start=1)
summary(a)
### Example with a vector argument:  Estimate the mean and            ###
### variance of a random normal sample by maximum likelihood          ###
### Note: you might want to use maxLik instead                        ###
loglik <- function(param) {
                           # param is a 2-vector of c(mean, sd)
  mu <- param[1]
  sigma <- param[2]
  ll <- -0.5*N*log(2*pi) - N*log(sigma) - sum(0.5*(x - mu)^2/sigma^2)
  ll
}
x <- rnorm(100, 1, 2) # use mean=1, sd=2
N <- length(x)
res <- maxNR(loglik, start=c(0,1)) # use 'wrong' start values
summary(res)
### The previous example with named parameters and a fixed value      ###
resFix <- maxNR(loglik, start=c(mu=0, sigma=1), fixed="sigma")
summary(resFix)  # 'sigma' is exactly 1.000 now.
### Constrained optimization                                          ###
### We maximize exp(-x^2 - y^2) where x+y = 1                         ###
hatf <- function(theta) {
  x <- theta[1]
  y <- theta[2]
  exp(-(x^2 + y^2))
  ## Note: you may prefer exp(- theta %*% theta) instead
}
### use constraints: x + y = 1                                        ###
A <- matrix(c(1, 1), 1, 2)
B <- -1
res <- maxNR(hatf, start=c(0,0), constraints=list(eqA=A, eqB=B),
             control=list(printLevel=1))
print(summary(res))