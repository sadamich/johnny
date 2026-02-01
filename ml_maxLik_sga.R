### Package maxLik ###
### Source: https://cran.r-project.org/web/packages/maxLik/refman/maxLik.html ###
library(maxLik)
### estimate the exponential distribution parameter by ML                  ###
set.seed(1)
t <- rexp(100, 2)
loglik <- function(theta, index) sum(log(theta) - theta*t[index])
### Note the log-likelihood and gradient are summed over observations      ###
gradlik <- function(theta, index) sum(1/theta - t[index])
### Estimate with full-batch                                               ###
a <- maxSGA(loglik, gradlik, start=1, control=list(iterlim=1000,
            SG_batchSize=10), nObs=100)
            # note that loglik is not really needed, and is not used
            # here, unless more print verbosity is asked
summary(a)
### demonstrate the usage of index, and using                              ###
### fn for computing the objective function on validation data.            ###
### Create a linear model where variables are very unequally scaled        ###
### OLS loglik function: compute the function value on validation data only###
loglik <- function(beta, index) {
   e <- yValid - XValid %*% beta
   -crossprod(e)/length(y)
}
### OLS gradient: compute it on training data only                         ###
### Use 'index' to select the subset corresponding to the minibatch        ###
gradlik <- function(beta, index)  {
   e <- yTrain[index] - XTrain[index,,drop=FALSE] %*% beta
   g <- t(-2*t(XTrain[index,,drop=FALSE]) %*% e)
   -g/length(index)
}
N <- 1000
### two random variables: one with scale 1, the other with 100             ###
X <- cbind(rnorm(N), rnorm(N, sd=100))
beta <- c(1, 1)  # true parameter values
y <- X %*% beta + rnorm(N, sd=0.2)
### training-validation split                                              ###
iTrain <- sample(N, 0.8*N)
XTrain <- X[iTrain,,drop=FALSE]
XValid <- X[-iTrain,,drop=FALSE]
yTrain <- y[iTrain]
yValid <- y[-iTrain]
### do this without momentum: learning rate must stay small for the gradient ###
### not to explode
cat("  No momentum:\n")
a <- maxSGA(loglik, gradlik, start=c(10,10),
           control=list(printLevel=1, iterlim=50,
                        SG_batchSize=30, SG_learningRate=0.0001, SGA_momentum=0
                        ), nObs=length(yTrain))
print(summary(a))  # the first component is off, the second one is close to the true value
### do with momentum 0.99
cat("  Momentum 0.99:\n")
a <- maxSGA(loglik, gradlik, start=c(10,10),
           control=list(printLevel=1, iterlim=50,
                        SG_batchSize=30, SG_learningRate=0.0001, SGA_momentum=0.99
                        # no momentum
                        ), nObs=length(yTrain))
print(summary(a))  # close to true value