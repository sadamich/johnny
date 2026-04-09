### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm535<- read.csv("xm535.csv", header =TRUE)
str(xm535)
attach(xm535)
detach(xm535)

### Problem a Aussumptions 1-7                                             ###
The stability of parameter
The stochastic variables
The zero mean of disturbances
The homogeneity 
The no autocorrelation of disturbances
The linearity of the model
The normality of distribution of disturbances
### Problem b The Exhibit 5 49 
### Problem c the subset of small profit                                   ###

xm535_s<- xm535[order(xm535$LOGPROFIT), ]
detach(xm535)
attach(xm535_s)
eq_small<- lm(LOGSALARY[1:48] ~ LOGPROFIT[1:48])
summary(eq_small)
### The firms with small profit Call: Profit is not significant            ###
### lm(formula = LOGSALARY[1:48] ~ LOGPROFIT[1:48])
Residuals:
     Min       1Q   Median       3Q      Max 
-0.31224 -0.24974 -0.07577  0.16741  0.63256 
Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)      6.86961    0.22662  30.314   <2e-16 ***
LOGPROFIT[1:48]  0.03682    0.04889   0.753    0.455    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.2736 on 46 degrees of freedom
Multiple R-squared:  0.01218,   Adjusted R-squared:  -0.009294 
F-statistic: 0.5672 on 1 and 46 DF,  p-value: 0.4552
library(strucchange)
eq_cusum<- efp(LOGSALARY[1:48] ~ LOGPROFIT[1:48],type = "Rec-CUSUM")
plot(eq_cusum)
sctest(eq_cusum)
Recursive CUSUM test
data:  eq_cusum
S = 0.49656, p-value = 0.6343
res_small<- resid(eq_small)
plot(res_small, type ="l")
eq_large<- lm(LOGSALARY[49:100] ~ LOGPROFIT[49:100])
summary(eq_large)
### The firms with large profits Call:                                    ###
### lm(formula = LOGSALARY[49:100] ~ LOGPROFIT[49:100])                   ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.76572 -0.23606  0.03254  0.28946  0.87336 
Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        6.55932    0.28525  22.995  < 2e-16 ***
LOGPROFIT[49:100]  0.13918    0.04146   3.357  0.00159 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3552 on 46 degrees of freedom
  (4 observations deleted due to missingness)
Multiple R-squared:  0.1968,    Adjusted R-squared:  0.1793 
F-statistic: 11.27 on 1 and 46 DF,  p-value: 0.001589
eq_cusum2<- efp(LOGSALARY[49:100] ~ LOGPROFIT[49:100],type = "Rec-CUSUM")
plot(eq_cusum2)
sctest(eq_cusum2)
res_large<- resid(eq_large)
plot(res_large, type ="l")