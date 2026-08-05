https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-OOP.pdf

set.seed(123)
x <- rnorm(250)
y <- rnbinom(250, mu = exp(1 + x), size = 1)

### A negative binomial model :nbinom(

fm_pois <- glm(y ~ x + I(x^2), family = poisson)
library(lmtest)
coeftest(fm_pois)

z test of coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  1.063268   0.041357 25.7094  < 2e-16 ***
x            0.996072   0.053534 18.6062  < 2e-16 ***
I(x^2)      -0.049124   0.023146 -2.1223  0.03381 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

library(sandwich)
coeftest(fm_pois, vcov = sandwich)
z test of coefficients:

             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  1.063268   0.083776 12.6918   <2e-16 ***
x            0.996072   0.105217  9.4668   <2e-16 ***
I(x^2)      -0.049124   0.036284 -1.3539   0.1758    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

fm_qpois <- glm(y ~ x + I(x^2), family = quasipoisson)
coeftest(fm_qpois)
z test of coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  1.063268   0.090435 11.7572   <2e-16 ***
x            0.996072   0.117063  8.5088   <2e-16 ***
I(x^2)      -0.049124   0.050613 -0.9706   0.3318    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

library(MASS)
fm_nbin <- glm.nb(y ~ x + I(x^2))
coeftest(fm_nbin)
z test of coefficients:

             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  1.066057   0.088574 12.0358   <2e-16 ***
x            0.999616   0.094894 10.5340   <2e-16 ***
I(x^2)      -0.052652   0.064883 -0.8115   0.4171    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 