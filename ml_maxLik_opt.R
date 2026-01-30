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