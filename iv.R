### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm531<- read.csv("xm531.csv", header = TRUE)
str(xm531)
attach(xm531)
panel01<- lm(GC ~ PG+RI)
summary(panel01)
### Panel 1 Call:lm(formula = GC ~ PG + RI)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.046139 -0.013715 -0.000357  0.020523  0.038915 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.98600    0.08110   61.48   <2e-16 ***
PG          -0.52758    0.02632  -20.05   <2e-16 ***
RI           0.57322    0.02451   23.39   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02385 on 27 degrees of freedom
Multiple R-squared:  0.9872,    Adjusted R-squared:  0.9862 
F-statistic:  1037 on 2 and 27 DF,  p-value: < 2.2e-16

panel03<- lm(PG ~ RPT+RPN+RPU+RI)
summary(panel03)
### Panel 3 (p.404) Call:lm(formula = PG ~ RPT + RPN + RPU + RI)           ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.160161 -0.036024 -0.002891  0.034824  0.175059 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   7.7410     0.8337   9.285 1.40e-09 ***
RPT          -0.8080     0.1912  -4.225 0.000277 ***
RPN          -3.5279     0.3520 -10.023 3.06e-10 ***
RPU           0.2331     0.1831   1.273 0.214765    
RI           -2.2984     0.2471  -9.303 1.35e-09 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07269 on 25 degrees of freedom
Multiple R-squared:  0.8868,    Adjusted R-squared:  0.8687 
F-statistic: 48.97 on 4 and 25 DF,  p-value: 1.797e-11

pg_fit<- fitted(panel03)
panel02<- lm(GC ~ pg_fit + RI)
summary(panel02)
### Panel 2 (p.404) Call:lm(formula = GC ~ pg_fit + RI)                    ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.054154 -0.025003 -0.008033  0.012243  0.090373 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.01370    0.13386   37.46  < 2e-16 ***
pg_fit      -0.54445    0.04618  -11.79 3.72e-12 ***
RI           0.56466    0.04050   13.94 7.46e-14 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.03833 on 27 degrees of freedom
Multiple R-squared:  0.9668,    Adjusted R-squared:  0.9644 
F-statistic: 393.3 on 2 and 27 DF,  p-value: < 2.2e-16

### Example 5 30 (p.400)                                                   ###
xm511<- read.csv("xm511.csv", header = TRUE)
str(xm511)
attach(xm511)
DUS3MT_lag1<- c(NA,lag(DUS3MT))
DUS3MT_lag2<- c(NA,NA,lag(DUS3MT))
str(DUS3MT)
DUS3MT_80<- DUS3MT[361:600]
str(DUS3MT_80)
### Panel 1 (p.401) Autocorrelation                                        ###
acf(DUS3MT_80)
DUS3MT_lag180<- DUS3MT_lag1[361:600]
DUS3MT_lag280<- DUS3MT_lag2[361:600]
panel02<- lm(DUS3MT_80~ DUS3MT_lag180+DUS3MT_lag280)
summary(panel02)
### Panel 2 (p.401) Call:lm(formula = DUS3MT_80 ~ DUS3MT_lag180            ###
###                                             + DUS3MT_lag280)           ###
Residuals:
    Min      1Q  Median      3Q     Max 
-3.5101 -0.1137  0.0235  0.1866  2.4483 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   -0.02611    0.03901  -0.669    0.504    
DUS3MT_lag180  0.35815    0.06231   5.748 2.76e-08 ***
DUS3MT_lag280 -0.28260    0.06227  -4.539 9.01e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.6035 on 237 degrees of freedom
Multiple R-squared:  0.1517,    Adjusted R-squared:  0.1445 
F-statistic: 21.18 on 2 and 237 DF,  p-value: 3.436e-09
res_aux<- resid(panel02)
xhat<- fitted(panel02)
DAAA_80<- DAAA[361:600]
panel04<- lm(DAAA_80~ xhat)
summary(panel04)
### Panel 4 (p.401) Call:lm(formula = DAAA_80 ~ xhat)                     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-1.13311 -0.15251 -0.00666  0.16657  1.31431 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.008453   0.020060  -0.421   0.6739  
xhat         0.169779   0.078626   2.159   0.0318 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3088 on 238 degrees of freedom
Multiple R-squared:  0.01921,   Adjusted R-squared:  0.01509 
F-statistic: 4.663 on 1 and 238 DF,  p-value: 0.03183
res_aux<- resid(panel04)
panel_4<- lm(DAAA_80 ~ xhat -1)
summary(panel_4)
### Panel 4 (p.408) Call:lm(formula = DAAA_80 ~ xhat - 1)                ###
Residuals:
     Min       1Q   Median       3Q      Max 
-1.14073 -0.16093 -0.01501  0.15807  1.30620 
Coefficients:
     Estimate Std. Error t value Pr(>|t|)  
xhat   0.1735     0.0780   2.224   0.0271 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3083 on 239 degrees of freedom
Multiple R-squared:  0.02028,   Adjusted R-squared:  0.01618 
F-statistic: 4.947 on 1 and 239 DF,  p-value: 0.02708

anova(panel04, panel_4)
Analysis of Variance Table
Model 1: DAAA_80 ~ xhat
Model 2: DAAA_80 ~ xhat - 1
  Res.Df    RSS Df Sum of Sq      F Pr(>F)
1    238 22.700                           
2    239 22.716 -1 -0.016935 0.1776 0.6739 (H0 -constant- is not rejectet.)

eqols<- lm(DAAA_80 ~ DUS3MT_80)
summary(eqols)
res_ols<- resid(eqols)
res_aux<- resid(panel02)
hausman<- lm(res_ols ~ DUS3MT_80+ res_aux)
summary(hausman)
### Panel 1 (p.415) Call:lm(formula = res_ols ~ DUS3MT_80 + res_aux)      ###
### Hausman test                                                          ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.67573 -0.13130  0.00406  0.12481  1.02532 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.003895   0.015359  -0.254   0.8000  
DUS3MT_80   -0.136674   0.060199  -2.270   0.0241 *
res_aux      0.161106   0.065359   2.465   0.0144 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2365 on 237 degrees of freedom
Multiple R-squared:  0.025,     Adjusted R-squared:  0.01677 
F-statistic: 3.038 on 2 and 237 DF,  p-value: 0.0498
### LM = n*rR^2 = 240 * 0.025 = 6 (H0 - exogeneity is rejectet)           ###

eqsargan<- lm(  ~ DUS3MT_lag180+DUS3MT_lag280)
summary(eqsargan)
