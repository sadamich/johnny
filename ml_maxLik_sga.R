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

Stochastic Gradient Ascent 
Number of iterations: 1000 
Return code: 4 
Iteration limit exceeded (iterlim) 
Function value: 
Estimates:
     estimate   gradient
[1,] 2.099567 -0.1279617

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
Stochastic Gradient Ascent 
Number of iterations: 50 
Return code: 4 
Iteration limit exceeded (iterlim) 
Function value: -816.6898 
Estimates:
     estimate     gradient
[1,] 8.056228    -22.18443
[2,] 1.568811 -10806.06327

### https://cran.r-project.org/web/packages/maxLik/vignettes/stochastic-gradient-maxLik.pdf
### 4 Example usage: Linear regression
### 4.1 Setting Up
library(MASS)
str(Boston)
'data.frame':   506 obs. of  14 variables:
 $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
 $ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
 $ indus  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
 $ chas   : int  0 0 0 0 0 0 0 0 0 0 ...
 $ nox    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
 $ rm     : num  6.58 6.42 7.18 7 7.15 ...
 $ age    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
 $ dis    : num  4.09 4.97 4.97 6.06 6.06 ...
 $ rad    : int  1 2 2 3 3 3 5 5 5 5 ...
 $ tax    : num  296 242 242 222 222 222 311 311 311 311 ...
 $ ptratio: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
 $ black  : num  397 397 393 395 397 ...
 $ lstat  : num  4.98 9.14 4.03 2.94 5.33 ...
 $ medv   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ...

i <- which(names(MASS::Boston) == "medv")
X <- as.matrix(MASS::Boston[,-i])
str(X)
 num [1:506, 1:13] 0.00632 0.02731 0.02729 0.03237 0.06905 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:506] "1" "2" "3" "4" ...
  ..$ : chr [1:13] "crim" "zn" "indus" "chas" ...
X <- cbind("const"=1, X) # add constant
str(X)
 num [1:506, 1:14] 1 1 1 1 1 1 1 1 1 1 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:506] "1" "2" "3" "4" ...
  ..$ : chr [1:14] "const" "crim" "zn" "indus" ...

y <- MASS::Boston[,i]
str(y)
    num [1:506] 24 21.6 34.7 33.4 36.2 28.7 22.9 


colMeans(X)
      const         crim           zn        indus         chas          nox 
  1.00000000   3.61352356  11.36363636  11.13677866   0.06916996   0.55469506 
          rm          age          dis          rad          tax      ptratio 
  6.28463439  68.57490119   3.79504269   9.54940711 408.23715415  18.45553360 
       black        lstat 
356.67403162  12.65306324        ### 14 columns  ###

z<- t(X)%*%X
ev<- eigen(z)
(values<- ev$values)
(vectors<- ev$vectors)

betaX<- solve(z)%*% t(X)%*%y
betaX<- drop(betaX) # matrix to vector
betaX

gradloss <- function(theta, index) {
e <- y[index]- X[index,,drop=FALSE] %*% theta
 }

set.seed(3)
start <- setNames(rnorm(ncol(X), sd=0.1), colnames(X))
# add names for better reference
res <- try(maxSGA(grad=gradloss,
start=start,
nObs=nrow(X),
control=list(iterlim=1000)
)
)


res <- maxSGA(grad=gradloss,
start=start,
nObs=nrow(X),
control=list(iterlim=1000,
SG_clip=1e4) # limit ||g|| <= 100
)

summary(res)