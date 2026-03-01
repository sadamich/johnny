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
Time series regression with "ts" data:
Start = 5, End = 600
Call:dynlm(formula = as.formula(model.text), data = data, start = 1)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.60753 -0.06599 -0.00631  0.06631  0.83990 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.004932   0.006562   0.752 0.452562    
X.t          0.240319   0.015220  15.789  < 2e-16 ***
X.1         -0.084940   0.018270  -4.649 4.12e-06 ***
X.2          0.080345   0.018584   4.323 1.81e-05 ***
X.3         -0.061725   0.018466  -3.343 0.000883 ***
X.4          0.055952   0.014559   3.843 0.000135 ***
Y.1          0.376830   0.042374   8.893  < 2e-16 ***
Y.2         -0.229086   0.045060  -5.084 4.98e-07 ***
Y.3          0.087533   0.042700   2.050 0.040813 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1599 on 587 degrees of freedom
Multiple R-squared:  0.4596,    Adjusted R-squared:  0.4522 
F-statistic:  62.4 on 8 and 587 DF,  p-value: < 2.2e-16
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
eq_garch<- garch(res_adl, order = c(1,1))
res_garch<- resid(eq_garch)
summary(eq_garch)
### Panel 5 Error correction model (p.653)                                 ###
DUS3MT_50<- DUS3MT[25:624]
DAAA_50<- DAAA[25:624]
AAA_1<- AAA[24:623]
US3MTBIL_1<- US3MTBIL[24:623]
const<- rep(1,600)
A<- AAA_1+US3MTBIL_1 + const
panel05<- lm(DAAA_50 ~ DUS3MT_50+(AAA_1 +US3MTBIL_1 + const) -1)
summary(panel05)
