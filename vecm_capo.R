https://cran.r-project.org/web/packages/urca/refman/urca.html#ca.po
library(urca)
data(ecb)
str(ecb)
'data.frame':   26 obs. of  5 variables:
 $ period  : Factor w/ 26 levels "Q1-1998","Q1-1999",..: 13 20 1 7 14 21 2 8 15 22 ...
 $ gdp.defl: num  95.9 96.5 96.9 97.4 97.8 98 98.1 98.6 98.7 98.9 ...
 $ gdp.nom : num  1442 1464 1475 1489 1507 ...
 $ m3      : num  4170 4224 4268 4338 4374 ...
 $ rl      : num  5.66 5.46 5.01 4.91 4.27 3.95 4.18 4.53 5.24 5.3 ...

m3.real <- ecb[,"m3"]/ecb[,"gdp.defl"]
gdp.real <- ecb[,"gdp.nom"]/ecb[,"gdp.defl"]
rl <- ecb[,"rl"]
ecb.data <- cbind(m3.real, gdp.real, rl)

m3d.po <- ca.po(ecb.data, type="Pz")
summary(m3d.po)

######################################## 
# Phillips and Ouliaris Unit Root Test # 
######################################## 
Test of type Pz 
detrending of series none 

Response m3.real :

Call:
lm(formula = m3.real ~ zr - 1)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.57878 -0.26119 -0.00787  0.20350  1.06522 

Coefficients:
           Estimate Std. Error t value Pr(>|t|)    
zrm3.real   0.99210    0.04147  23.920   <2e-16 ***
zrgdp.real  0.08627    0.16348   0.528    0.603    
zrrl       -0.09819    0.16534  -0.594    0.559    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3617 on 22 degrees of freedom
Multiple R-squared:      1,     Adjusted R-squared:  0.9999 
F-statistic: 1.584e+05 on 3 and 22 DF,  p-value: < 2.2e-16


Response gdp.real :

Call:
lm(formula = gdp.real ~ zr - 1)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.088196 -0.039820  0.005241  0.044033  0.091465 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
zrm3.real  -0.020408   0.005853  -3.487  0.00209 ** 
zrgdp.real  1.073163   0.023070  46.518  < 2e-16 ***
zrrl       -0.022009   0.023334  -0.943  0.35581    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.05104 on 22 degrees of freedom
Multiple R-squared:      1,     Adjusted R-squared:      1 
F-statistic: 8.523e+05 on 3 and 22 DF,  p-value: < 2.2e-16


Response rl :

Call:
lm(formula = rl ~ zr - 1)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.57215 -0.20139  0.04127  0.17227  0.59364 

Coefficients:
           Estimate Std. Error t value Pr(>|t|)    
zrm3.real  -0.05566    0.03661  -1.521 0.142616    
zrgdp.real  0.27091    0.14429   1.878 0.073765 .  
zrrl        0.64773    0.14594   4.438 0.000207 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3192 on 22 degrees of freedom
Multiple R-squared:  0.9962,    Adjusted R-squared:  0.9956 
F-statistic:  1906 on 3 and 22 DF,  p-value: < 2.2e-16
Value of test-statistic is: 18.4658 

Critical values of Pz are:
                  10pct    5pct    1pct
critical values 62.1436 71.2751 89.6679

> 
