### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

library(tsDyn)
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
detach(xm722)
y<- ts(US3MTBIL, freq=12, start=1948)
plot(y, main="Time series", ylab="US3MTBIL")
grid<-selectSETAR(y, m=1, thDelay=0, trim=0.15, criterion="SSR")
Using maximum autoregressive order for low regime: mL = 1 
Using maximum autoregressive order for high regime: mH = 1 
Searching on 287 possible threshold values within regimes with sufficient ( 15% ) number of observations
Searching on 287 combinations of thresholds (287) and thDelay (1) 

set<-setar(y,m=1,thDelay=0,th=grid$th)
summary(set)
Non linear autoregressive model

SETAR model ( 2 regimes)
Coefficients:
Low regime:
   const.L     phiL.1 
0.04125161 0.99254900 

High regime:
  const.H    phiH.1 
0.5734113 0.9361319 

Threshold:
-Variable: Z(t) = + (1) X(t)
-Value: 6.81 (fixed)
Proportion of points in low regime: 76.24%       High regime: 23.76% 

Residuals:
       Min         1Q     Median         3Q        Max 
-4.5292578 -0.1335745 -0.0096437  0.1436366  2.9571110 

Fit:
residuals variance = 0.2133,  AIC = -956, MAPE = 4.975%

Coefficient(s):

         Estimate  Std. Error  t value  Pr(>|t|)    
const.L  0.041252    0.052861   0.7804 0.4354638    
phiL.1   0.992549    0.012836  77.3243 < 2.2e-16 ***
const.H  0.573411    0.155943   3.6770 0.0002564 ***
phiH.1   0.936132    0.016458  56.8814 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Threshold
Variable: Z(t) = + (1) X(t) 

Value: 6.81 (fixed)

