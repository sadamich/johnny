### Package maxLik ###
### Source: https://cran.r-project.org/web/packages/maxLik/refman/maxLik.html ###
library(maxLik)
### estimate the exponential distribution parameter by ML                  ###
### random variable of exponential distribution 
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

Initial function value: -144514.8 
Iteration limit exceeded (iterlim) 
50  iterations
estimate: 7.70482733075221, 1.17322863612906 
Function value: -60.63036 


print(summary(a))  # the first component is off, the second one is close to the true value
Stochastic Gradient Ascent 
Number of iterations: 50 
Return code: 4 
Iteration limit exceeded (iterlim) 
Function value: -60.63036 
Estimates:
     estimate   gradient
[1,] 7.704827   -14.1322
[2,] 1.173229 -3258.3841


### do with momentum 0.99
cat("  Momentum 0.99:\n")
a <- maxSGA(loglik, gradlik, start=c(10,10),
           control=list(printLevel=1, iterlim=50,
           SG_batchSize=30, SG_learningRate=0.0001, SGA_momentum=0.99
                        # no momentum
                        ), nObs=length(yTrain))
Initial function value: -144514.8 
Iteration limit exceeded (iterlim) 
50  iterations
estimate: 0.981617257496255, 0.984151627705168 
Function value: -0.4634487 

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


betaX<- solve(crossprod(X)) %*% crossprod(X, y)
betaX<- drop(betaX) # matrix to vector
betaX
   const          crim            zn         indus          chas 
 3.645949e+01 -1.080114e-01  4.642046e-02  2.055863e-02  2.686734e+00 
          nox            rm           age           dis           rad 
-1.776661e+01  3.809865e+00  6.922246e-04 -1.475567e+00  3.060495e-01 
          tax       ptratio         black         lstat 
-1.233459e-02 -9.527472e-01  9.311683e-03 -5.247584e-01 

gradloss <- function(theta, index) {
e <- y[index]- X[index,,drop=FALSE] %*% theta
g <- t(e) %*% X[index,,drop=FALSE]
           2*g/length(index)
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

Iteration 63 
Parameter:
[1] 3.47655620157556e+298, 1.55792823742485e+299, 3.46679084058245e+299, 4.20148533450887e+299, 2.37932965325502e+297, 1.97831609274068e+298, 2.17080583489071e+299 ...
Gradient:
               [,1]           [,2]           [,3]           [,4]           [,5]
[1,] -2.176452e+304 -9.753202e+304 -2.170338e+305 -2.630284e+305 -1.489548e+303
               [,6]           [,7]
[1,] -1.238498e+304 -1.359004e+305
 reached getOption("max.cols") -- omitted 7 columns
Error in maxSGACompute(fn = function (theta, fnOrig, gradOrig, hessOrig,  : 
  NA/Inf in gradient


res <- maxSGA(grad=gradloss,
start=start,
nObs=nrow(X),
control=list(iterlim=1000,
SG_clip=1e4) # limit ||g|| <= 100
)

summary(res)

Stochastic Gradient Ascent 
Number of iterations: 1000 
Return code: 4 
Iteration limit exceeded (iterlim) 
Function value: 
Estimates:
           estimate      gradient
const   -0.07999887 -1.749115e-05
crim     0.02785691 -7.755669e-05
zn       0.22208281 -1.754769e-04
indus    0.06456437 -2.106458e-04
chas     0.02077633 -1.198114e-06
nox      0.01196464 -9.941313e-06
rm       0.11108882 -1.092356e-04
age      1.20485974 -1.245784e-03
dis     -0.06026450 -6.282687e-05
rad      0.28567967 -1.921515e-04
tax      6.62820142 -7.689873e-03
ptratio  0.18507316 -3.261922e-04
black    5.81629057 -6.246515e-03
lstat    0.22176090 -2.326626e-04
--------------------------------------------

loss <- function(theta, index) {
e <- y[index]- X[index,] %*% theta
   -crossprod(e)/length(index)
}


res <- maxSGA(loss, gradloss,
start=start,
nObs=nrow(X),
control=list(iterlim=1000,
# will misbehave with larger numbers
SG_clip=1e4,
SG_learningRate=0.001,
storeParameters=TRUE,
storeValues=TRUE
)
)

par <- storedParameters(res)
val <- storedValues(res)
par(mfrow=c(1,2))
plot(par[,1], par[,2], type="b", pch=".",
xlab=names(start)[1], ylab=names(start)[2], main="Parameters")
## add some arrows to see which way the parameters move
iB <- c(40, nrow(par)/2, nrow(par))
iA <- iB- 10
arrows(par[iA,1], par[iA,2], par[iB,1], par[iB,2], length=0.1)
##
plot(seq(length=length(val))-1,-val, type="l",
xlab="epoch", ylab="MSE", main="Loss",
log="y")

### 4 2 Training and Validation sets 

i <- sample(nrow(X), 0.8*nrow(X)) # training indices, 80% of data
Xt <- X[i,] # training data
yt <- y[i]
Xv <- X[-i,] # validation data
yv <- y[-i]


gradloss <- function(theta, index) {
e <- yt[index]- Xt[index,,drop=FALSE] %*% theta
g <- 2*t(e) %*% Xt[index,,drop=FALSE]
      -g/length(index)
}

loss <- function(theta, index) {
e <- yv- Xv %*% theta
-crossprod(e)/length(yv)
}



res <- maxSGA(loss, gradloss,
start=start,
nObs=nrow(Xt), # note: only training data now
control=list(iterlim=100,
SG_batchSize=1,
SG_learningRate=1e-5,
SG_clip=1e4,
storeParameters=TRUE,
storeValues=TRUE
)
)

par <- storedParameters(res)
val <- storedValues(res)
par(mfrow=c(1,2))
plot(par[,1], par[,2], type="b", pch=".",
xlab=names(start)[1], ylab=names(start)[2], main="Parameters")
 iB <- c(40, nrow(par)/2, nrow(par))
iA <- iB- 1
arrows(par[iA,1], par[iA,2], par[iB,1], par[iB,2], length=0.1)

plot(seq(length=length(val))-1,-val, type="l",
xlab="epoch", ylab="MSE", main="Loss",
log="y")



res <- maxSGA(loss, gradloss,
start=start,
nObs=nrow(Xt),
control=list(iterlim=100,
SG_batchSize=1,
SG_learningRate=1e-6,
SG_clip=1e4,
SGA_momentum = 0.99,
storeParameters=TRUE,
storeValues=TRUE
)
)

par <- storedParameters(res)
val <- storedValues(res)
par(mfrow=c(1,2))
plot(par[,1], par[,2], type="b", pch=".",
    xlab=names(start)[1], ylab=names(start)[2], main="Parameters")


iB <- c(40, nrow(par)/2, nrow(par))
iA <- iB- 1
 arrows(par[iA,1], par[iA,2], par[iB,1], par[iB,2], length=0.1)

 plot(seq(length=length(val))-1,-val, type="l",
  xlab="epoch", ylab="MSE", main="Loss", log="y")



res <- maxAdam(loss, gradloss,
start=start,
nObs=nrow(Xt),
control=list(iterlim=100,
SG_batchSize=1,
SG_learningRate=1e-6,
SG_clip=1e4,
storeParameters=TRUE,
storeValues=TRUE
)
)


par <- storedParameters(res)
val <- storedValues(res)
par(mfrow=c(1,2))
plot(par[,1], par[,2], type="b", pch=".",
xlab=names(start)[1], ylab=names(start)[2], main="Parameters")
iB <- c(40, nrow(par)/2, nrow(par))
iA <- iB- 1
arrows(par[iA,1], par[iA,2], par[iB,1], par[iB,2], length=0.1)
plot(seq(length=length(val))-1,-val, type="l",
xlab="epoch", ylab="MSE", main="Loss",
log="y")


### 4 3 Sequence of batch sizes

val <- NULL
# loop over batch sizes
for(B in c(1,10,100)) {

res <- maxAdam(loss, gradloss,
start=start,
nObs=nrow(Xt),
control=list(iterlim=200,
SG_batchSize=1,
SG_learningRate=1e-6,
SG_clip=1e4,
SG_patience=5,
# worse value allowed only 5 times
storeValues=TRUE )
)
cat("Batch size", B, ",", nIter(res),
"epochs, function value", maxValue(res), "\n")
val <- c(val, na.omit(storedValues(res)))
start <- coef(res)
}
Batch size 1 , 6 epochs, function value -9562.199 
Batch size 10 , 6 epochs, function value -9562.199 
Batch size 100 , 6 epochs, function value -9562.199 

plot(seq(length=length(val))-1,-val, type="l",
xlab="epoch", ylab="MSE", main="Loss",
log="y")
summary(res)

Stochastic Gradient Ascent/Adam 
Number of iterations: 6 
Return code: 10 
Lost patience (SG_patience) 
Function value: -9562.199 
Estimates:
            estimate      gradient
const   -0.126693661 -1.315825e-05
crim    -0.041604939 -4.571544e-05
zn       0.007896147  0.000000e+00
indus   -0.141442527 -2.381643e-04
chas     0.009374608 -1.315825e-05
nox     -0.027019552 -9.447622e-06
rm      -0.021607324 -1.155294e-04
age      0.083454958 -1.090819e-03
dis     -0.149897503 -2.506251e-05
rad      0.102878604 -3.157979e-04
tax     -0.103707917 -8.763393e-03
ptratio -0.143457846 -2.657966e-04
black   -0.101131181 -4.665257e-03
lstat   -0.001377560 -6.960713e-05


