https://cran.r-project.org/web/packages/car/refman/car.html#Boot
Bootstrapping for regression models
library(car)
m1 <- lm(Fertility ~ ., swiss)
summary(m1)
### OLS Call:
lm(formula = Fertility ~ ., data = swiss)
Residuals:
     Min       1Q   Median       3Q      Max 
-15.2743  -5.2617   0.5032   4.1198  15.3213 
Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      66.91518   10.70604   6.250 1.91e-07 ***
Agriculture      -0.17211    0.07030  -2.448  0.01873 *  
Examination      -0.25801    0.25388  -1.016  0.31546    
Education        -0.87094    0.18303  -4.758 2.43e-05 ***
Catholic          0.10412    0.03526   2.953  0.00519 ** 
Infant.Mortality  1.07705    0.38172   2.822  0.00734 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 7.165 on 41 degrees of freedom
Multiple R-squared:  0.7067,    Adjusted R-squared:  0.671 
F-statistic: 19.76 on 5 and 41 DF,  p-value: 5.594e-10


betahat.boot <- Boot(m1, R=199) # 199 bootstrap samples--too small to be useful
summary(betahat.boot)  # default summary
Number of bootstrap replications R = 199 
                 original   bootBias    bootSE  bootMed
(Intercept)      66.91518  0.1541859 10.171639 67.02350
Agriculture      -0.17211 -0.0018003  0.059737 -0.17409
Examination      -0.25801 -0.0146538  0.243497 -0.27317
Education        -0.87094  0.0263323  0.275524 -0.87445
Catholic          0.10412 -0.0033973  0.033002  0.10043
Infant.Mortality  1.07705  0.0046954  0.416919  1.07673
confint(betahat.boot)
Bootstrap bca confidence intervals

                       2.5 %      97.5 %
(Intercept)      45.18538168 87.20816523
Agriculture      -0.28698246 -0.05539651
Examination      -0.72271970  0.19601336
Education        -1.29395962 -0.13351926
Catholic          0.04199331  0.17878702
Infant.Mortality  0.27866949  2.00729112

hist(betahat.boot)
# Bootstrap for the estimated residual standard deviation:
sigmahat.boot <- Boot(m1, R=199, f=sigmaHat, labels="sigmaHat")
summary(sigmahat.boot)
 R original bootBias  bootSE bootMed
sigmaHat 199   7.1654  -0.5165 0.65991   6.647

confint(sigmahat.boot)
Bootstrap bca confidence intervals

            2.5 %   97.5 %
sigmaHat 6.411332 8.910025
Warning message:
In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints
> 
