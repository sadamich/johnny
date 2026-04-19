### https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-OOP.pdf ###
### 4 1 Bread 
### https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-CL.pdf ###
### 3 Methods 3 1 Sandwich covariances
Sandwich
Bread    Hessian
Meat     Gradient
### 3 2 Clustered variancies

https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#sandwich
x <- sin(1:10)
y <- rnorm(10)
fm <- lm(y ~ x)
sandwich(fm)
            (Intercept)           x
(Intercept) 0.043265676 0.008950109
x           0.008950109 0.079979393

vcovHC(fm, type = "HC")
            (Intercept)           x
(Intercept) 0.043265676 0.008950109
x           0.008950109 0.079979393

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
