library(maxLik)
## Create a 'maxControl' object:
maxControl(tol=1e-4, sann_tmax=7, printLevel=2)

## Optimize quadratic form t(D) %*% W %*% D with p.d. weight matrix,
## s.t. constraints sum(D) = 1
quadForm <- function(D) {
   return(-t(D) %*% W %*% D)
}
quadForm
eps <- 0.1
W <- diag(3) + matrix(runif(9), 3, 3)*eps
str(W)
 num [1:3, 1:3] 1.00691 0.00592 0.03137 0.00262 1.07185 ...
D <- rep(1/3, 3)
D                        # initial values
## create control object and use it for optimization
co <- maxControl(printLevel=2, qac="marquardt", marquardt_lambda0=1)
co
res <- maxNR(quadForm, start=D, control=co)
print(summary(res))

## Now perform the same with no trace information
co <- maxControl(co, printLevel=0)
co
res <- maxNR(quadForm, start=D, control=co) # no tracing information
print(summary(res))  # should be the same as above
maxControl(res) # shows the control structure