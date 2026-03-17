### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr318<- read.csv("xr318.csv", header =TRUE)
str(xr318)
attach(xr318)
y<- log(SGAS/PGAS)
x2<- log(INC/PALL)
x3<- log(PGAS/PALL)
x4<- log(PPUB/PALL)
x5<- log(PNCAR/PALL)
x6<- log(PUCAR/PALL)
eq1<- lm(log(SGAS) ~ log(PGAS))
summary(eq1)
### The price elasticity Call:lm(formula = log(SGAS) ~ log(PGAS))          ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.36869 -0.11865  0.05744  0.10314  0.32208 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.04677    0.35054   17.25  < 2e-16 ***
log(PGAS)    1.19513    0.07785   15.35 3.65e-15 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1867 on 28 degrees of freedom
Multiple R-squared:  0.8938,    Adjusted R-squared:   0.89 
F-statistic: 235.7 on 1 and 28 DF,  p-value: 3.649e-15
###  missleading????
eq2<- lm(y ~ log(PGAS))
summary(eq2)
### The price elasticity Call:lm(formula = y ~ log(PGAS))                  ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.36869 -0.11865  0.05744  0.10314  0.32208 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.04677    0.35054  17.250   <2e-16 ***
log(PGAS)    0.19513    0.07785   2.507   0.0183 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1867 on 28 degrees of freedom
Multiple R-squared:  0.1833,    Adjusted R-squared:  0.1541 
F-statistic: 6.283 on 1 and 28 DF,  p-value: 0.01827
eq3<- lm(y~ x2+x3)
summary(eq3)

### Call:lm(formula = y ~ x2 + x3)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.046139 -0.013715 -0.000357  0.020523  0.038915 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.98600    0.08110   61.48   <2e-16 ***
x2           0.57322    0.02451   23.39   <2e-16 ***
x3          -0.52758    0.02632  -20.05   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02385 on 27 degrees of freedom
Multiple R-squared:  0.9872,    Adjusted R-squared:  0.9862 
F-statistic:  1037 on 2 and 27 DF,  p-value: < 2.2e-16
eq4<- lm(y~ x3)
summary(eq4)

### Call:lm(formula = y ~ x3)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.25263 -0.05369  0.02944  0.07735  0.15338 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.87975    0.02029 339.097  < 2e-16 ***
x3          -0.86281    0.09993  -8.634 2.22e-09 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.108 on 28 degrees of freedom
Multiple R-squared:  0.727,     Adjusted R-squared:  0.7172 
F-statistic: 74.55 on 1 and 28 DF,  p-value: 2.217e-09


eq_f<- lm(y ~ x2+x3+x4+x5+x6)
summary(eq_f)
### Call:lm(formula = y ~ x2 + x3 + x4 + x5 + x6)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.047914 -0.012814  0.004161  0.014504  0.035373 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.99234    0.55229   7.229 1.81e-07 ***
x2           0.86605    0.16392   5.284 2.03e-05 ***
x3          -0.43960    0.06282  -6.998 3.10e-07 ***
x4           0.03351    0.07864   0.426    0.674    
x5           0.39029    0.24766   1.576    0.128    
x6          -0.09467    0.05935  -1.595    0.124    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02283 on 24 degrees of freedom
Multiple R-squared:  0.9895,    Adjusted R-squared:  0.9874 
F-statistic: 453.8 on 5 and 24 DF,  p-value: < 2.2e-16
