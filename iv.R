### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm531<- read.csv("xm531.csv", header = TRUE)
str(xm531)
attach(xm531)
detach(xm531)
panel01<- lm(GC ~ PG+RI)
res_ols<- resid(panel01)
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
res_aux<- resid(panel03)
eq_hausman<- lm(res_ols~PG+RI+res_aux)
summary(eq_hausman)
### Hausman test Exogeneity Panel 1 (p.417)                                ### 
### lm(formula = res_ols ~ PG + RI + res_aux)                              ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.041418 -0.015382 -0.002321  0.016155  0.042308 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)
(Intercept)  0.027703   0.081429   0.340    0.736
PG          -0.016872   0.028093  -0.601    0.553
RI          -0.008558   0.024638  -0.347    0.731
res_aux      0.104845   0.070032   1.497    0.146
Residual standard error: 0.02332 on 26 degrees of freedom
Multiple R-squared:  0.07936,   Adjusted R-squared:  -0.02686 
F-statistic: 0.7471 on 3 and 26 DF,  p-value: 0.5339
LM=n*R^2= 30* 0.07936 =  2.3808

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
res_iv<- resid(panel02)
eq_sargan<- lm(res_iv~ RPT+RPN+RPU+RI)
summary(eq_sargan)
### Compare the panel 2 (p.416)lm(formula = res_iv ~ RPT + RPN + RPU + RI) ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.067822 -0.020037 -0.005752  0.022093  0.084996 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.20975    0.44738  -0.469    0.643
RPT         -0.05120    0.10261  -0.499    0.622
RPN          0.02041    0.18887   0.108    0.915
RPU         -0.07023    0.09826  -0.715    0.481
RI           0.06041    0.13258   0.456    0.653
Residual standard error: 0.03901 on 25 degrees of freedom
Multiple R-squared:  0.04093,   Adjusted R-squared:  -0.1125 
F-statistic: 0.2667 on 4 and 25 DF,  p-value: 0.8965
LM= n*R^2=30* 0.04093=1.2279

const<- rep(1,30)
z<- c(const,RPT,RPN,RPU,RI)
library("gmm")
eq_tsls<- tsls(GC~pg_fit+RI, x=z, data=xm531)
summary(eq_tsls)
### Example 5 30 (p.400)                                                   ###
xm511<- read.csv("xm511.csv", header = TRUE)
str(xm511)
attach(xm511)
detach(xm511)
DUS3MT_lag1<- c(NA,lag(DUS3MT))
DUS3MT_lag2<- c(NA,NA,lag(DUS3MT))
str(DUS3MT)
DUS3MT_80<- DUS3MT[361:600]
str(DUS3MT_80)
### Panel 1 (p.401) Autocorrelation                                        ###
acf(DUS3MT_80)
DUS3MT_lag180<- DUS3MT_lag1[361:600]
DUS3MT_lag280<- DUS3MT_lag2[361:600]

### Panel 1 (p.408)                                                        ###
library(gmm)
panel02<- lm(DUS3MT_80~ DUS3MT_lag180+DUS3MT_lag280)
const<- rep(1, 240)
z<- cbind(const,DUS3MT_lag180, DUS3MT_lag280)
library("gmm")
eq_tsls<- tsls(DAAA[361:600]~DUS3MT[361:600], x=z)
summary(eq_tsls)
### Panel 1 (p.408) tsls(g = DAAA[361:600] ~ DUS3MT[361:600], x = z)       ###
Method:  Two Stage Least Squares(Meat type = Classical) 
Coefficients:
                 Estimate    Std. Error  t value     Pr(>|t|)  
(Intercept)      -0.0084530   0.0165717  -0.5100850   0.6099919
DUS3MT[361:600]   0.1697788   0.0649521   2.6139065   0.0089514
J-Test: degrees of freedom is 1 
                J-test    P-value 
Test E(g)=0:    0.032488  0.856962
 First stage F-statistics: 
DUS3MT[361:600] : F( 2 ,  237 ) =  21.18306  (P-Vavue =  3.436291e-09 )
res_tsls<- resid(eq_tsls)
eq_sargan<- lm(res_tsls~ DUS3MT[360:599]+DUS3MT[359:598])
summary(eq_sargan)
### Panel 6 Sargan test (p.415)                                            ###
### lm(formula = res_tsls ~ DUS3MT[360:599] + DUS3MT[359:598])             ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.74664 -0.13315  0.00007  0.13673  1.16679 
Coefficients:
                  Estimate Std. Error t value Pr(>|t|)
(Intercept)     -0.0001558  0.0165252  -0.009    0.992
DUS3MT[360:599] -0.0022183  0.0263953  -0.084    0.933
DUS3MT[359:598] -0.0033868  0.0263777  -0.128    0.898
Residual standard error: 0.2556 on 237 degrees of freedom
Multiple R-squared:  0.0001354, Adjusted R-squared:  -0.008302 
F-statistic: 0.01604 on 2 and 237 DF,  p-value: 0.9841
LM= n*R^2= 240*0.0001354 =  0.032496
eq_gmm<- gmm(DAAA[361:600]~DUS3MT[361:600], x=z)
summary(eq_gmm)
### Compare the panel 1(p.408)gmm(g = DAAA[361:600] ~ DUS3MT[361:600], x = z)###
Method:  twoStep 
Kernel:  Quadratic Spectral(with bw =  1.47311 )
Coefficients:
                 Estimate    Std. Error  t value     Pr(>|t|)  
(Intercept)      -0.0091059   0.0236358  -0.3852616   0.7000436
DUS3MT[361:600]   0.1691413   0.0867269   1.9502746   0.0511434
J-Test: degrees of freedom is 1 
                J-test    P-value 
Test E(g)=0:    0.010698  0.917622
Initial values of the coefficients
    (Intercept) DUS3MT[361:600] 
    -0.00845297      0.16977883 
### The Sargan test Panel 6 (p.415)                                        ###
res_iv<- resid(eq_gmm)
eq_sargan<- lm(res_iv~ DUS3MT[360:599]+DUS3MT[359:598])
summary(eq_sargan)
240*0.0001356 = 0.032544
panel01<- lm(DAAA[361:600] ~ DUS3MT[361:600])
res_ols<- resid(panel01)
### Eigenvalue decomposition???                                            ###
ev<- eigen(p)
(values<- ev$values)
(vectors<- ev$vectors)
ev
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

eqols<- lm(DUS3MT_80~ DUS3MT_lag180+DUS3MT_lag280)
summary(eqols)
xhat<- fitted(eqols)
eq_iv<- lm(DAAA_80~ xhat)
summary(eq_iv)
res_iv<- resid(eq_iv)
eqsargan<- lm(res_iv~ DUS3MT_lag180+ DUS3MT_lag280)
summary(eqsargan)
### Compare with the panel 6 (p.415)                                       ###
### lm(formula = res_iv ~ DUS3MT_lag180 + DUS3MT_lag280)                   ###
Residuals:
     Min       1Q   Median       3Q      Max 
-1.13759 -0.15194 -0.00699  0.16635  1.31507 
Coefficients:
                Estimate Std. Error t value Pr(>|t|)
(Intercept)   -0.0001558  0.0200047  -0.008    0.994
DUS3MT_lag180 -0.0022183  0.0319529  -0.069    0.945
DUS3MT_lag280 -0.0033868  0.0319316  -0.106    0.916
Residual standard error: 0.3095 on 237 degrees of freedom
Multiple R-squared:  9.238e-05, Adjusted R-squared:  -0.008346 
F-statistic: 0.01095 on 2 and 237 DF,  p-value: 0.9891
LM = n*R^2 = 240 * 0.00009238
[1] 0.0221712
### H0 is not rejectet                                                     ###


