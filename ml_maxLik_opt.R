### Package maxLik ###
### Source: https://cran.r-project.org/web/packages/maxLik/refman/maxLik.html ###
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