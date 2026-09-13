https://cran.r-project.org/web/packages/MASS/refman/MASS.html#gehan
library(MASS)
library(survival)
str(gehan)
'data.frame':   42 obs. of  4 variables:
 $ pair : int  1 1 2 2 3 3 4 4 5 5 ...
 $ time : int  1 10 22 7 3 32 12 23 8 22 ...
 $ cens : int  1 1 1 1 1 0 1 1 1 1 ...
 $ treat: Factor w/ 2 levels "6-MP","control": 2 1 2 1 2 1 2 1 2 1 ...

gehan.surv <- survfit(Surv(time, cens) ~ treat, data = gehan,
     conf.type = "log-log")
summary(gehan.surv)
survreg(Surv(time, cens) ~ factor(pair) + treat, gehan, dist = "exponential")
summary(survreg(Surv(time, cens) ~ treat, gehan, dist = "exponential"))

summary(survreg(Surv(time, cens) ~ treat, gehan))
gehan.cox <- coxph(Surv(time, cens) ~ treat, gehan)
summary(gehan.cox)

### leuk
https://cran.r-project.org/web/packages/MASS/refman/MASS.html#leuk
str(leuk)
'data.frame':   33 obs. of  3 variables:
 $ wbc : int  2300 750 4300 2600 6000 10500 10000 17000 5400 7000 ...
 $ ag  : Factor w/ 2 levels "absent","present": 2 2 2 2 2 2 2 2 2 2 ...
 $ time: int  65 156 100 134 16 108 121 4 39 143 ...
library(survival)
plot(survfit(Surv(time) ~ ag, data = leuk), lty = 2:3, col = 2:3)

# now Cox models
leuk.cox <- coxph(Surv(time) ~ ag + log(wbc), leuk)
summary(leuk.cox)
Call:
coxph(formula = Surv(time) ~ ag + log(wbc), data = leuk)

  n= 33, number of events= 33 

             coef exp(coef) se(coef)      z Pr(>|z|)   
agpresent -1.0691    0.3433   0.4293 -2.490  0.01276 * 
log(wbc)   0.3677    1.4444   0.1360  2.703  0.00687 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

          exp(coef) exp(-coef) lower .95 upper .95
agpresent    0.3433     2.9126     0.148    0.7964
log(wbc)     1.4444     0.6923     1.106    1.8857

Concordance= 0.726  (se = 0.047 )
Likelihood ratio test= 15.64  on 2 df,   p=4e-04
Wald test            = 15.06  on 2 df,   p=5e-04
Score (logrank) test = 16.49  on 2 df,   p=3e-04


