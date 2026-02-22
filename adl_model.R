### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 22 Interest and bond rates p.642 ###
xm722<- read.csv("xm722.csv", header = TRUE) 
str(xm722)
attach(xm722)
library("dLagM")
DUS3MT_50<- DUS3MT[25:624]
DAAA_50<- DAAA[25:624]
### panel 1 ADL (0,0) Model (p.645)                                          ###
eq1<- lm(DAAA_50 ~ DUS3MT_50)
summary(eq1)
Call: lm(formula = DAAA_50 ~ DUS3MT_50)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.70283 -0.07086 -0.00395  0.06571  1.06943 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.006393   0.006982   0.916     0.36    
DUS3MT_50   0.274585   0.014641  18.754   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.171 on 598 degrees of freedom
Multiple R-squared:  0.3703,    Adjusted R-squared:  0.3693 
F-statistic: 351.7 on 1 and 598 DF,  p-value: < 2.2e-16
### Panel 2 ADL (3,4) Model (p.645)                                        ###
eq_adl<- ardlDlm(x = DUS3MT_50, y = DAAA_50, p = 4, q= 3)
summary(eq_adl)
res<- resid(eq1)
res_adl<- resid(eq_adl)
### Panel 3 (p.645) 
acf(res)                                                                   ###
acf(res_adl)
res<- ts(res, freq = 12, start = 1950)
### Exhibit e clustered residuals (p.646)                                  ###
plot(res, main  ="Time series", ylab = "Residuals")
### Compare with Panel 7 GARCH (p.646)                                     ###
library(tseries)
garch(res_adl, order = c(1,1))


