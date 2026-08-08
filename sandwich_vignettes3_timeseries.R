https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf
library(sandwich)

### 4.2. Testing coefficients in time-series data
data("Investment")
str(Investment)

 Time-Series [1:20, 1:7] from 1963 to 1982: 597 638 691 756 800 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:7] "GNP" "Investment" "Price" "Interest" ...
head(Investment)
nterest   RealGNP  RealInv   RealInt
1963 596.7       90.9 0.7167     3.23  832.5659 126.8313        NA
1964 637.7       97.4 0.7277     3.55  876.3227 133.8464 2.0151877
1965 691.1      113.5 0.7436     4.04  929.3975 152.6358 1.8550337
1966 756.0      125.7 0.7676     4.50  984.8880 163.7572 1.2724583
1967 799.6      122.8 0.7906     4.19 1011.3838 155.3251 1.1936477
1968 873.4      133.3 0.8254     5.16 1058.1536 161.4975 0.7582798

fm.inv <- lm(RealInv ~ RealGNP + RealInt, data = Investment)
summary(fm.inv)

Call:
lm(formula = RealInv ~ RealGNP + RealInt, data = Investment)
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

library(car)
library(lmtest)
coeftest(fm.inv, df = Inf, vcov = NeweyWest(fm.inv, lag = 4, prewhite = FALSE))
t of coefficients:

              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -12.533601  18.958298 -0.6611   0.5085    
RealGNP       0.169136   0.016751 10.0972   <2e-16 ***
RealInt      -1.001438   3.342375 -0.2996   0.7645    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

 coeftest(fm.inv, df = Inf, vcov = NeweyWest)
t of coefficients:

              Estimate Std. Error z value  Pr(>|z|)    
(Intercept) -12.533601  24.374177 -0.5142    0.6071    
RealGNP       0.169136   0.023586  7.1709 7.449e-13 ***
RealInt      -1.001438   3.639935 -0.2751    0.7832    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

parzenHAC <- function(x, ...) kernHAC(x, kernel = "Parzen", prewhite = 2,
adjust = FALSE, bw = bwNeweyWest, ...)

 coeftest(fm.inv, df = Inf, vcov = parzenHAC)