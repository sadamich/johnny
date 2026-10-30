https://cran.r-project.org/web/packages/car/refman/car.html#boxCox
library(car)
attach(trees)
str(trees)
'data.frame':   31 obs. of  3 variables:
 $ Girth : num  8.3 8.6 8.8 10.5 10.7 10.8 11 11 11.1 11.2 ...
 $ Height: num  70 65 63 72 81 83 66 75 80 75 ...
 $ Volume: num  10.3 10.3 10.2 16.4 18.8 19.7 15.6 18.2 22.6 19.9

with(trees, boxCox(Volume ~ log(Height) + log(Girth), data = trees,
         lambda = seq(-0.25, 0.25, length = 10)))

attach(quine)
str(quine)
'data.frame':   146 obs. of  5 variables:
 $ Eth : Factor w/ 2 levels "A","N": 1 1 1 1 1 1 1 1 1 1 ...
 $ Sex : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
 $ Age : Factor w/ 4 levels "F0","F1","F2",..: 1 1 1 1 1 1 1 1 2 2 ...
 $ Lrn : Factor w/ 2 levels "AL","SL": 2 2 2 1 1 1 1 1 2 2 ...
 $ Days: int  2 11 14 5 5 13 20 22 6 6 ...
  data("quine", package = "MASS")
  with(quine, boxCox(Days ~ Eth*Sex*Age*Lrn, 
         lambda = seq(-0.05, 0.45, len = 20), family="yjPower"))

https://cran.r-project.org/web/packages/car/refman/car.html#boxCoxVariable
mod <- lm(interlocks + 1 ~ assets, data=Ornstein)
summary(mod)
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.038e+01  7.424e-01   13.98   <2e-16 ***
assets      7.029e-04  4.190e-05   16.77   <2e-16 ***
mod.aux <- update(mod, . ~ . + boxCoxVariable(interlocks + 1))
summary(mod.aux)
Coefficients:
                                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)                     1.461e+01  5.426e-01  26.920   <2e-16 ***
assets                         -7.142e-05  5.119e-05  -1.395    0.164    
boxCoxVariable(interlocks + 1)  7.427e-01  4.136e-02  17.956   <2e-16 ***