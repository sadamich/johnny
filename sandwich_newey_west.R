### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### 5 5 2 OLS and autocorrelation (p. 358-360)                             ###
https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#NeweyWest
library(sandwich)
## fit investment equation
data(Investment)
str(Investment)
fm <- lm(RealInv ~ RealGNP + RealInt, data = Investment)
summary(fm)

Call: lm(formula = RealInv ~ RealGNP + RealInt, data = Investment)
Residuals:
    Min      1Q  Median      3Q     Max 
-34.987  -6.638   0.180  10.408  26.288 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -12.53360   24.91527  -0.503    0.622    
RealGNP       0.16914    0.02057   8.224 3.87e-07 ***
RealInt      -1.00144    2.36875  -0.423    0.678    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 17.21 on 16 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.8141,    Adjusted R-squared:  0.7908 
F-statistic: 35.03 on 2 and 16 DF,  p-value: 1.429e-06

## Newey & West (1994) compute this type of estimator
NeweyWest(fm)
            (Intercept)       RealGNP     RealInt
(Intercept) 594.1004817 -0.5617817294 36.04992496
RealGNP      -0.5617817  0.0005563172 -0.04815937
RealInt      36.0499250 -0.0481593694 13.24912546

## The Newey & West (1987) estimator requires specification
## of the lag and suppression of prewhitening
NeweyWest(fm, lag = 4, prewhite = FALSE)
            (Intercept)       RealGNP      RealInt
(Intercept) 359.4170681 -0.3115505035 -4.089319305
RealGNP      -0.3115505  0.0002805888 -0.005355931
RealInt      -4.0893193 -0.0053559312 11.171472998


## bwNeweyWest() can also be passed to kernHAC(), e.g.
## for the quadratic spectral kernel
kernHAC(fm, bw = bwNeweyWest)
            (Intercept)       RealGNP     RealInt
(Intercept)  794.986166 -0.7562570101 48.19485118
RealGNP       -0.756257  0.0007537517 -0.06485461
RealInt       48.194851 -0.0648546058 17.58798679

https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#kweights
curve(kweights(x, kernel = "Quadratic", normalize = TRUE),
      from = 0, to = 3.2, xlab = "x", ylab = "k(x)")
curve(kweights(x, kernel = "Bartlett", normalize = TRUE),
      from = 0, to = 3.2, col = 2, add = TRUE)
curve(kweights(x, kernel = "Parzen", normalize = TRUE),
      from = 0, to = 3.2, col = 3, add = TRUE)
curve(kweights(x, kernel = "Tukey", normalize = TRUE),
      from = 0, to = 3.2, col = 4, add = TRUE)
curve(kweights(x, kernel = "Truncated", normalize = TRUE),
      from = 0, to = 3.2, col = 5, add = TRUE)

https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#weightsAndrews

## fit investment equation
data(Investment)
fm <- lm(RealInv ~ RealGNP + RealInt, data = Investment)

## compute quadratic spectral kernel HAC estimator
kernHAC(fm)
            (Intercept)       RealGNP     RealInt
(Intercept) 788.6120652 -0.7502080996 49.78912814
RealGNP      -0.7502081  0.0007483977 -0.06641343
RealInt      49.7891281 -0.0664134303 17.71735491
kernHAC(fm, verbose = TRUE)
Bandwidth chosen: 1.744749 
            (Intercept)       RealGNP     RealInt
(Intercept) 788.6120652 -0.7502080996 49.78912814
RealGNP      -0.7502081  0.0007483977 -0.06641343
RealInt      49.7891281 -0.0664134303 17.71735491
> 

## use Parzen kernel instead, VAR(2) prewhitening, no finite sample
## adjustment and Newey & West (1994) bandwidth selection
kernHAC(fm, kernel = "Parzen", prewhite = 2, adjust = FALSE,
  bw = bwNeweyWest, verbose = TRUE)
Bandwidth chosen: 2.814444 
            (Intercept)       RealGNP      RealInt
(Intercept) 608.3101258 -0.5089107386 -64.93690203
RealGNP      -0.5089107  0.0004340803   0.04689293
RealInt     -64.9369020  0.0468929322  15.58251456
## compare with estimate under assumption of spheric errors
vcov(fm)
 (Intercept)       RealGNP     RealInt
(Intercept) 620.7706170 -0.5038304429  8.47475285
RealGNP      -0.5038304  0.0004229789 -0.01145679
RealInt       8.4747529 -0.0114567949  5.61097245

https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#weightsLumley

x <- sin(1:100)
y <- 1 + x + rnorm(100)
fm <- lm(y ~ x)
weave(fm)
             (Intercept)            x
(Intercept)  0.008741632 -0.001365024
x           -0.001365024  0.015733250
vcov(fm)
             (Intercept)            x
(Intercept) 8.923630e-03 2.257536e-05
x           2.257536e-05 1.775197e-02
