### https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-OOP.pdf ###
### 4 1 Bread 
### https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-CL.pdf ###
### 3 Methods 3 1 Sandwich covariances
Sandwich
Bread    Hessian
Meat     Gradient
### 3 2 Clustered variancies
library(sandwich)
### https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#sandwich
x <- sin(1:10)
y <- rnorm(10)
fm <- lm(y ~ x)

### bread: n * (x'x)^{-1}
bread(fm)
            (Intercept)          x
(Intercept)   1.0414689 -0.2938577
x            -0.2938577  2.0823419

solve(crossprod(cbind(1, x))) * 10
                                 x
              1.0414689 -0.2938577
x            -0.2938577  2.0823419


### estfun()                                                               ###
## linear regression
x <- 1:9
y <- sin(1:9/5)
m <- lm(y ~ x)
## estimating function: (y - x'beta) * x
estfun(m)
 (Intercept)           x
1 -0.13577304 -0.13577304
2 -0.04481531 -0.08963062
3  0.03061755  0.09185265
4  0.08353989  0.33415956
5  0.10786351  0.53931755
6  0.09864034  0.59184202
7  0.05225971  0.36581794
8 -0.03340770 -0.26726157
9 -0.15892494 -1.43032449
residuals(m) * cbind(1, x)
                           x
 [1,] -0.13577304 -0.13577304
 [2,] -0.04481531 -0.08963062
 [3,]  0.03061755  0.09185265
 [4,]  0.08353989  0.33415956
 [5,]  0.10786351  0.53931755
 [6,]  0.09864034  0.59184202
 [7,]  0.05225971  0.36581794
 [8,] -0.03340770 -0.26726157
 [9,] -0.15892494 -1.43032449

### Meat 
x <- sin(1:10)
y <- rnorm(10)
fm <- lm(y ~ x)
meat(fm)
            (Intercept)         x
(Intercept)   0.6761770 0.3064227
x             0.3064227 0.2405474

meatHC(fm, type = "HC")
            (Intercept)         x
(Intercept)   0.6761770 0.3064227
x             0.3064227 0.2405474

meatHAC(fm)
            (Intercept)         x
(Intercept)   0.7773621 0.4312438
x             0.4312438 0.3694204
 

### sandwich 
x <- sin(1:10)
y <- rnorm(10)
fm <- lm(y ~ x)
B<- bread(fm)
M<- meat(fm)
sw<- B%*%M%*%B
sw
            (Intercept)          x
(Intercept)  0.73146773 0.03295184
x            0.03295184 1.16217262
sandwich(fm)
            (Intercept)           x
(Intercept) 0.073146773 0.003295184
x           0.003295184 0.116217262

vcovHC(fm, type = "HC")
            (Intercept)           x
(Intercept) 0.073146773 0.003295184
x           0.003295184 0.116217262       

https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#vcovHAC
vcovHAC(x, ...)
x <- sin(1:100)
y <- 1 + x + rnorm(100)
fm <- lm(y ~ x)
vcovHAC(fm)
             (Intercept)            x
(Intercept)  0.010403554 -0.001237466
x           -0.001237466  0.018663833
vcov(fm)
             (Intercept)            x
(Intercept) 1.039060e-02 2.628656e-05
x           2.628656e-05 2.067024e-02

x <- sin(1:100)
y <- 1 + x + rnorm(100)
## model fit and HC3 covariance
fm <- lm(y ~ x)
vcovHC(fm)
               Intercept)             x
(Intercept)  0.0083294468 -0.0006571695
x           -0.0006571695  0.0150817943
## usual covariance matrix
vcovHC(fm, type = "const")
vcov(fm)
             (Intercept)            x
(Intercept) 8.179993e-03 2.069408e-05
x           2.069408e-05 1.627264e-02

sigma2 <- sum(residuals(lm(y ~ x))^2)/98
sigma2 * solve(crossprod(cbind(1, x)))
                          x
  8.179993e-03 2.069408e-05
x 2.069408e-05 1.627264e-02
