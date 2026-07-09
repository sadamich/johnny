### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

library(tsDyn)
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
detach(xm722)
x<- ts(AAA, freq=12, start=1948)
plot(x, main="Time series", ylab="AAA")
grid<-selectSETAR(AAA, m=1, thDelay=0, trim=0.15, criterion="SSR")

set<-setar(AAA,m=1,thDelay=0,th=grid$th)
### Warning message:
### Possible unit root in the low  regime. Roots are: 0.9891 
summary(set)

Non linear autoregressive model

SETAR model ( 2 regimes)
Coefficients:
Low regime:
    const.L      phiL.1 
-0.02780505  1.01099171 

High regime:
   const.H     phiH.1 
0.06766316 0.99236629 

Threshold:
-Variable: Z(t) = + (1) X(t)
-Value: 7.14 (fixed)
Proportion of points in low regime: 47.83%       High regime: 52.17% 
Residuals:
       Min         1Q     Median         3Q        Max 
-1.1301040 -0.0640743 -0.0034114  0.0686355  1.3069947 

Fit:
residuals variance = 0.04418,  AIC = -1939, MAPE = 1.614%
Coefficient(s):

          Estimate  Std. Error  t value Pr(>|t|)    
const.L -0.0278050   0.0400836  -0.6937   0.4881    
phiL.1   1.0109917   0.0088151 114.6885   <2e-16 ***
const.H  0.0676632   0.0556380   1.2161   0.2244    
phiH.1   0.9923663   0.0058167 170.6061   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Threshold
Variable: Z(t) = + (1) X(t) 
Value: 7.14 (fixed)
> 



