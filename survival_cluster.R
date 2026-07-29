https://cran.r-project.org/web/packages/survival/refman/survival.html#cluster

library(survival)
litter:	litter number from 1 to 100
rx:	treatment,(1=drug, 0=control)
time:	time to tumor or last follow-up
status:	event status, 1=tumor and 0=censored
sex:	male or female

str(rats)
'data.frame':   300 obs. of  5 variables:
 $ litter: int  1 1 1 2 2 2 3 3 3 4 ...
 $ rx    : num  1 0 0 1 0 0 1 0 0 1 ...
 $ time  : num  101 49 104 91 104 102 104 102 104 91 ...
 $ status: num  0 1 0 0 0 0 0 0 0 0 ...
 $ sex   : chr  "f" "f" "f" "m" ...

z<- marginal.model <- coxph(Surv(time, status) ~ rx, data= rats, cluster=litter,
                         subset=(sex=='f'))

summary(z)
### Call:
coxph(formula = Surv(time, status) ~ rx, data = rats, subset = (sex == 
    "f"), cluster = litter)

  n= 150, number of events= 40 

     coef exp(coef) se(coef) robust se     z Pr(>|z|)   
rx 0.9047    2.4713   0.3175    0.3025 2.991  0.00278 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

   exp(coef) exp(-coef) lower .95 upper .95
rx     2.471     0.4046     1.366     4.471

Concordance= 0.586  (se = 0.037 )
Likelihood ratio test= 7.98  on 1 df,   p=0.005
Wald test            = 8.94  on 1 df,   p=0.003
Score (logrank) test = 8.68  on 1 df,   p=0.003,   Robust = 7.65  p=0.006

  (Note: the likelihood ratio and score tests assume independence of
     observations within a cluster, the Wald and robust score tests do not).


z2<- frailty.model  <- coxph(Surv(time, status) ~ rx + frailty(litter), rats,
                         subset=(sex=='f'))
summary(z2)

coxph(formula = Surv(time, status) ~ rx + frailty(litter), data = rats, 
    subset = (sex == "f"))

  n= 150, number of events= 40 

                coef   se(coef) se2    Chisq DF    p     
rx              0.9143 0.323    0.3189  8.01  1.00 0.0046
frailty(litter)                        17.69 14.41 0.2400

   exp(coef) exp(-coef) lower .95 upper .95
rx     2.495     0.4008     1.325     4.699

Iterations: 6 outer, 24 Newton-Raphson
     Variance of random effect= 0.4987319   I-likelihood = -180.8 
Degrees of freedom for terms=  1.0 14.4 
Concordance= 0.791  (se = 0.033 )
Likelihood ratio test= 37.65  on 15.38 df,   p=0.001

