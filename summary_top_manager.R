### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 35 Salaries of top managers (p.419)                          ###
xm535<- read.csv("xm535.csv", header = TRUE)
str(xm535)
attach(xm535)
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
### http://127.0.0.1:27442/library/base/html/sort.html ###
LOGPROFIT_s<- sort(LOGPROFIT)
str(LOGPROFIT_s)
LOGSALARY_s<- LOGSALARY[PROFIT > 0]
LOGSALARY_s<- na.omit(LOGSALARY_s)
LOGSALARY_s<- sort(LOGSALARY_s)
eq_s<- lm(LOGSALARY_s~ LOGPROFIT_s)
summary(eq_s)
res_s<- resid(eq_s)
plot(res_s, type="l")
fit<- fitted(eq_s)
fit_sq<- fit^2
eq_reset<- lm(LOGSALARY_s ~ LOGPROFIT_s + fit_sq)
summary(eq_reset)

str(fit)
fit_sq<- fit^s
panel05<- lm(LOGSALARY ~ LOGPROFIT + fit_sq)
summary(panel05)
res_sq<- res^2
logprofit_sq<- logprof^2
panel011<- lm(res_sq ~ logprof+ logprofit_sq)
summary(panel011)

panel09<- lm(LOGSALARY[1:77]~LOGPROFIT[1:77])
summary(panel09)
Call:lm(formula = LOGSALARY[1:77] ~ LOGPROFIT[1:77])
Residuals:
     Min       1Q   Median       3Q      Max 
-0.56110 -0.21552 -0.02492  0.19416  0.84531 
Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)       6.6015     0.1335  49.455  < 2e-16 ***
LOGPROFIT[1:77]   0.1368     0.0218   6.274 2.33e-08 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2882 on 72 degrees of freedom
  (3 observations deleted due to missingness)
Multiple R-squared:  0.3535,    Adjusted R-squared:  0.3445 
F-statistic: 39.37 on 1 and 72 DF,  p-value: 2.333e-08

anova(panel03, panel09)
Analysis of Variance Table
Response: LOGSALARY
          Df  Sum Sq Mean Sq F value    Pr(>F)    
LOGPROFIT  1  5.8212  5.8212  54.446 6.278e-11 ***
Residuals 94 10.0502  0.1069                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"LOGSALARY[1:77]"’ removed because response differs from model 1
> 


LOGSAL100<- LOGSALARY[78:100]
LOGPROFIT100<- LOGPROFIT[78:100]
panel09a<- lm(LOGSAL100~ LOGPROFIT100)
summary(panel09a)
Call:
Panel09a lm(formula = LOGSAL100 ~ LOGPROFIT100)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.068399 -0.029394 -0.007086  0.013509  0.084560
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   6.800453   0.043619 155.907   <2e-16 ***
LOGPROFIT100 -0.002348   0.008897  -0.264    0.795    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.04399 on 20 degrees of freedom
 (1 個の観測値が欠損のため削除されました)
Multiple R-squared:  0.003472,  Adjusted R-squared:  -0.04635 
F-statistic: 0.06968 on 1 and 20 DF,  p-value: 0.7945
Chow break point test
(((0.327)^2-(0.2882)^2-(0.04399)^2)/2)/((0.2882+0.04399)/(77+23-4))
chow break forecast test
((0.327-0.2882)/23)/(0.2882/(77-4))

White test
str(res)
res_100<- c(res, NA, NA,NA,NA)
res_100_sq<- res_100^2
LOGPROFIT_sq<- LOGPROFIT^2
panel11<- lm(res_100_sq~ LOGPROFIT+ LOGPROFIT_sq)
summary(panel11)

Breusch Godfrey test
res_lag1<- c(NA, lag(res_100))[1:100]
res_lag2<- c(NA, NA, lag(res_100))[1:100]
panel12<- lm(res_100~ LOGPROFIT+res_lag1+ res_lag2)
summary(panel12)

acf(res, 10)
str(LOGPROFIT)
logprof_iv<- lm(LOGPROFIT ~ LOGTURNOVER)
Profit<- fitted(logprof_iv)
str(Profita)
Profita<- c(Profit, NA,NA, NA, NA, NA,NA,NA, NA,NA,NA, NA,NA,NA,NA,NA,NA)
LOGSALARYa<- LOGSALARY[1:84]
panel016a<- lm(LOGSALARY~ Profit)
summary(panel016a)
Call:
lm(formula = LOGSALARY ~ Profit)

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
panel017<- lm(res~ 
LOGPROFIT + res_v)
summary(panel017)
Call:
lm(formula = res ~ LOGPROFIT + res_v)
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
