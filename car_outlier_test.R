https://cran.r-project.org/web/packages/car/refman/car.html#outlierTest
library(car)
attach(Duncan)
str(Duncan)
'data.frame':   45 obs. of  4 variables:
 $ type     : Factor w/ 3 levels "bc","prof","wc": 2 2 2 2 2 2 2 2 3 2 ...
 $ income   : int  62 72 75 55 64 21 64 80 67 72 ...
 $ education: int  86 76 92 90 86 84 93 100 87 86 ...
 $ prestige : int  82 83 90 76 90 87 93 90 52 88 ...
hist(income)
hist(education)
hist(prestige)
outlierTest(lm(prestige ~ income + education, data=Duncan))
No Studentized residuals with Bonferroni p < 0.05
Largest |rstudent|:
         rstudent unadjusted p-value Bonferroni p
minister 3.134519          0.0031772      0.14297