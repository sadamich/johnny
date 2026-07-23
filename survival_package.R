https://cran.r-project.org/web/packages/survival/refman/survival.html#survreg
library("survival")

### Fit an exponential model: the two fits are the same                ###
s<- survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist='weibull',
                                    scale=1)
summary(s)
### Call:survreg(formula = Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, 
    dist = "weibull", scale = 1)
             Value Std. Error     z       p
(Intercept)  6.962      1.322  5.27 1.4e-07
ecog.ps     -0.433      0.587 -0.74    0.46
rx           0.582      0.587  0.99    0.32
Scale fixed at 1 
Weibull distribution
Loglik(model)= -97.2   Loglik(intercept only)= -98
        Chisq= 1.67 on 2 degrees of freedom, p= 0.43 
Number of Newton-Raphson Iterations: 4 
n= 26 

s1<- survreg(Surv(futime, fustat) ~ 1, ovarian, dist='weibull',
                                    scale=1)
summary(s1)
### Call: survreg(formula = Surv(futime, fustat) ~ 1, data = ovarian, dist = "weibull", 
    scale = 1)
            Value Std. Error    z      p
(Intercept) 7.169      0.289 24.8 <2e-16
Scale fixed at 1 
Weibull distribution
Loglik(model)= -98   Loglik(intercept only)= -98
Number of Newton-Raphson Iterations: 4 
n= 26 
### the scale is 0                                                        ###
s2<- survreg(Surv(futime, fustat) ~ 1, ovarian, dist='weibull',
                                    scale=0)
summary(s2)
### survreg(formula = Surv(futime, fustat) ~ 1, data = ovarian,
### dist = "weibull", cale = 0)
             Value Std. Error    z      p
(Intercept)  7.111      0.293 24.3 <2e-16
Log(scale)  -0.103      0.254 -0.4   0.69
Scale= 0.902 
Weibull distribution
Loglik(model)= -98   Loglik(intercept only)= -98
Number of Newton-Raphson Iterations: 5 
n= 26 

### futime:	survival or censoring time
### fustat:	censoring status
### age:	in years
### resid.ds:	residual disease present (1=no,2=yes)
### rx:	treatment group
### ecog.ps:	ECOG performance status (1 is better, see reference)

str(ovarian)
'data.frame':   26 obs. of  6 variables:
 $ futime  : num  59 115 156 421 431 448 464 475 477 563 ...
 $ fustat  : num  1 1 1 0 1 0 1 1 0 1 ...
 $ age     : num  72.3 74.5 66.5 53.4 50.3 ...
 $ resid.ds: num  2 2 2 2 2 1 2 2 2 1 ...
 $ rx      : num  1 1 1 2 1 1 2 2 1 2 ...
 $ ecog.ps : num  1 1 2 1 1 2 2 2 1 2 ...

Call:survreg(formula = Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, 
    dist = "weibull", scale = 1)
Coefficients:
(Intercept)     ecog.ps          rx 
  6.9618376  -0.4331347   0.5815027 
Scale fixed at 1 
Loglik(model)= -97.2   Loglik(intercept only)= -98
        Chisq= 1.67 on 2 degrees of freedom, p= 0.434 
n= 26 

### Exponential distribution 
survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian,
        dist="exponential")
Call:survreg(formula = Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, 
    dist = "exponential")

Coefficients:
(Intercept)     ecog.ps          rx 
  6.9618376  -0.4331347   0.5815027 
Scale fixed at 1 
Loglik(model)= -97.2   Loglik(intercept only)= -98
        Chisq= 1.67 on 2 degrees of freedom, p= 0.434 
n= 26 
