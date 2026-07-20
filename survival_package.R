https://cran.r-project.org/web/packages/survival/refman/survival.html#survreg
library("survival")

# Fit an exponential model: the two fits are the same
survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist='weibull',
                                    scale=1)
survreg(Surv(futime, fustat) ~ 1, ovarian, dist='weibull',
                                    scale=1)
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
