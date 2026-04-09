### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 35 Salaries of top managers (p.419)                          ###
xm535<- read.csv("xm535.csv", header = TRUE)
str(xm535)
attach(xm535)
detach(xm535)
plot(PROFIT, SALARY)
plot(LOGPROFIT, LOGSALARY)
panel03<- lm(LOGSALARY ~ LOGPROFIT)
summary(panel03)
Panel03 Call:lm(formula = LOGSALARY ~ LOGPROFIT)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.71420 -0.24633 -0.04024  0.23518  0.94679

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.35034    0.12896  49.242  < 2e-16 ***
LOGPROFIT    0.16221    0.02198   7.379 6.28e-11 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.327 on 94 degrees of freedom
  (4 observations deleted due to missingness)
Multiple R-squared:  0.3668,    Adjusted R-squared:   0.36 
F-statistic: 54.45 on 1 and 94 DF,  p-value: 6.278e-11
res<- resid(panel03)
plot(res, type="l")
plot(LOGSALARY, type ="l")
LOGPROFIT_s<- sort(LOGPROFIT, index.return = TRUE)
ix<- LOGPROFIT_s$ix
str(LOGPROFIT_s)
LOGSALARY_s<- LOGSALARY[ix]
LOGSALARY_s<- as.numeric(LOGSALARY_s)
LOGPROFIT_s<- sort(LOGPROFIT)
eq_s<- lm(LOGSALARY_s~ LOGPROFIT_s)
summary(eq_s)
res_s<- resid(eq_s)
### Exhibit 5 49 (d) (p.420)                                               ###
plot(res_s, type="l")
fit<- fitted(eq_s)
fit_sq<- fit^2
eq_reset<- lm(LOGSALARY_s ~ LOGPROFIT_s + fit_sq)
summary(eq_reset)
str(fit)
fit_sq<- fit^s
### Panel 5 RESET Test(p.421)lm(formula=LOGSALARY_s ~ LOGPROFIT_s + fit_sq)###
Residuals:   Min       1Q   Median       3Q      Max 
-0.68812 -0.26622 -0.04463  0.22185  0.94482 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -6.0953    13.7992  -0.442    0.660
LOGPROFIT_s  -0.5717     0.8078  -0.708    0.481
fit_sq        0.3124     0.3452   0.905    0.368
Residual standard error: 0.3249 on 93 degrees of freedom
Multiple R-squared:  0.3663,    Adjusted R-squared:  0.3527 
F-statistic: 26.88 on 2 and 93 DF,  p-value: 6.128e-10

library(strucchange)
eq_cusum<- efp(LOGSALARY_s~ LOGPROFIT_s,type = "Rec-CUSUM")
plot(eq_cusum)
sctest(eq_cusum
### Chow break test
panel09<- lm(LOGSALARY_s[5:76]~LOGPROFIT_s[5:76])
summary(panel09)
### Compare with the Panel 9 Chow forecast Test (p.421)                    ###
### lm(formula = LOGSALARY_s[5:76] ~ LOGPROFIT_s[5:76])                    ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.52369 -0.26386 -0.05426  0.22140  0.91544 
Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)         6.2051     0.2871  21.617  < 2e-16 ***
LOGPROFIT_s[5:76]   0.1922     0.0548   3.508 0.000794 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3335 on 70 degrees of freedom
Multiple R-squared:  0.1495,    Adjusted R-squared:  0.1373 
F-statistic:  12.3 on 1 and 70 DF,  p-value: 0.0007945

anova(panel03, panel09)
Analysis of Variance Table
Response: LOGSALARY
          Df  Sum Sq Mean Sq F value    Pr(>F)    
LOGPROFIT  1  5.8212  5.8212  54.446 6.278e-11 ***
Residuals 94 10.0502  0.1069                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
models with response ‘"LOGSALARY_s[5:76]"’ removed because response 
differs from model 1



res_s<- resid(eq_s)
plot(res_s, type="l")
LOGPROFIT_sq<- LOGPROFIT_s^2
panel11<- lm(res_s^2~ LOGPROFIT_s+ LOGPROFIT_sq)
summary(panel11)
### White Test: Compare with the panel 11 (p.422)                          ###
### lm(formula = res_s^2 ~ LOGPROFIT_s + LOGPROFIT_sq)                     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.11175 -0.08575 -0.03419  0.04261  0.74345 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.036526   0.135894  -0.269    0.789
LOGPROFIT_s   0.050192   0.044944   1.117    0.267
LOGPROFIT_sq -0.004206   0.003614  -1.164    0.247
Residual standard error: 0.1347 on 93 degrees of freedom
Multiple R-squared:  0.0145,    Adjusted R-squared:  -0.006699 
F-statistic: 0.6839 on 2 and 93 DF,  p-value: 0.5071


res_lag1<- c(NA, lag(res_s))[1:96]
res_lag2<- c(NA, NA, lag(res_s))[1:96]
panel12<- lm(res_s~ LOGPROFIT_s+res_lag1+ res_lag2)
summary(panel12)
### Breusch Godfrey test: Compare with the panel 12 (p.422)                ###
### lm(formula = res_s ~ LOGPROFIT_s + res_lag1 + res_lag2)                ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.75330 -0.26132 -0.03266  0.22821  0.85875 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.08058    0.13859  -0.581    0.562
LOGPROFIT_s  0.01295    0.02341   0.553    0.582
res_lag1    -0.15577    0.10468  -1.488    0.140
res_lag2     0.03132    0.10501   0.298    0.766
Residual standard error: 0.3238 on 90 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared:  0.02909,   Adjusted R-squared:  -0.003273 
F-statistic: 0.8989 on 3 and 90 DF,  p-value: 0.4451

### Panel 13 (p. 422)                                                      ###
acf(res_s, 10)
hist(res_s)
summary(res_s)

### Now no ordered data 
xm535_s<- xm535[order(xm535$LOGPROFIT), ]
xm535_s                                                   
### Exhibit 5 49 (o) (p. 423)  Positive correlation                        ###
plot(LOGTURNOVER,LOGPROFIT)
logprof_iv<- lm(LOGPROFIT ~ LOGTURNOVER)
summary(logprof_iv)
Profit<- fitted(logprof_iv)
str(Profita)
Profita<- c(Profit, NA,NA, NA, NA, NA,NA,NA, NA,NA,NA, NA,NA,NA,NA,NA,NA)
LOGSALARYa<- LOGSALARY[Profit >0]
panel016a<- lm(LOGSALARYa~ Profit)
summary(panel016a)
### IV estimation: Panel 16 (p. 423) lm(formula = LOGSALARY ~ Profit)      ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.64599 -0.28874 -0.03064  0.23432  1.20376 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.25344    0.20332  30.757  < 2e-16 ***
Profit       0.18156    0.03548   5.117 2.01e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3534 on 82 degrees of freedom
MultipleR-squared:  0.242,     Adjusted R-squared:  0.2328 
F-statistic: 26.18 on 1 and 82 DF,  p-value: 2.01e-06

eq_ols<- lm(LOGSALARY ~ LOGPROFIT)
res<- resid(eq_ols)
res_v<- resid(logprof_iv)
panel017<- lm(res~ LOGPROFIT + res_v)
summary(panel017)
### Hausman Test: Panel 17 (p. 423) lm(formula = res ~ LOGPROFIT + res_v)  ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.74797 -0.22827 -0.02471  0.23661  0.93408 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.006064   0.184116  -0.033    0.974
LOGPROFIT    0.001078   0.032132   0.034    0.973
res_v       -0.002833   0.052095  -0.054    0.957
Residual standard error: 0.32 on 81 degrees of freedom
Multiple R-squared:  3.651e-05, Adjusted R-squared:  -0.02465 
F-statistic: 0.001479 on 2 and 81 DF,  p-value: 0.9985
