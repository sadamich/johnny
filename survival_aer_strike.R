library(AER)
data("StrikeDuration")
str(StrikeDuration)
'data.frame':   62 obs. of  2 variables:
 $ duration: num  7 9 13 14 26 29 52 130 9 37 ...
 $ uoutput : num  0.0114 0.0114 0.0114 0.0114 0.0114
hist(duration, freq=FALSE)
hist(uoutput, freq=FALSE)
summary(uoutput)
  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.10443 -0.03143  0.01138  0.01102  0.06450  0.07427 

attach(StrikeDuration)
library("MASS")
https://cran.r-project.org/web/packages/MASS/refman/MASS.html#fitdistr
## Greene (2003), Table 22.10
fit_exp <- fitdistr(StrikeDuration$duration, "exponential")
fit_exp
 rate    
  0.023431595 
 (0.002975816)

fit_wei <- fitdistr(StrikeDuration$duration, "weibull")
fit_wei
 shape         scale   
   0.92468222   41.10764952 
 ( 0.09156075) ( 5.96082380)
fit_wei$estimate[2]^(-1)
 scale 
0.02432637 

fit_lnorm <- fitdistr(StrikeDuration$duration, "lognormal")
fit_lnorm
 meanlog      sdlog  
  3.1044563   1.2840534 
 (0.1630749) (0.1153114)
1/fit_lnorm$estimate[2]
  sdlog 
0.7787838 
exp(-fit_lnorm$estimate[1])
 meanlog 
0.0448489 
## Weibull and lognormal distribution have
## different parameterizations, see Greene p. 794

## Greene (2003), Example 22.10
library("survival")
fm_wei <- survreg(Surv(duration) ~ uoutput, dist = "weibull", data = StrikeDuration)
summary(fm_wei)

### MASS : moters
https://cran.r-project.org/web/packages/MASS/refman/MASS.html#motors
library(MASS)
plot(survfit(Surv(duration) ~ factor(uoutput), StrikeDuration), conf.int = FALSE)
# fit Weibull model
strike.wei <- survreg(Surv(duration) ~ uoutput, StrikeDuration)
summary(strike.wei)
unlist(predict(strike.wei, data.frame(uoutput=0), se.fit = TRUE))
    fit.1  se.fit.1 
43.806148  5.988352 : the neutral case
unlist(predict(strike.wei, data.frame(uoutput=-0.1), se.fit = TRUE))
    fit.1  se.fit.1 
111.38514  39.19535 : the worst case
unlist(predict(strike.wei, data.frame(uoutput=0.07), se.fit = TRUE))
    fit.1  se.fit.1 
22.794524  4.982435 : the best case

strike.cox <- coxph(Surv(duration) ~ uoutput, StrikeDuration)
summary(strike.cox)
Call:
coxph(formula = Surv(duration) ~ uoutput, data = StrikeDuration)
  n= 62, number of events= 62 

             coef exp(coef)  se(coef)     z Pr(>|z|)   
uoutput     9.216 10060.629     3.229 2.855  0.00431 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
        exp(coef) exp(-coef) lower .95 upper .95
uoutput     10061   9.94e-05     17.96   5634413

Concordance= 0.608  (se = 0.041 )
Likelihood ratio test= 8.43  on 1 df,   p=0.004
Wald test            = 8.15  on 1 df,   p=0.004
Score (logrank) test = 8.31  on 1 df,   p=0.004


plot(survfit(strike.cox, newdata = data.frame(uoutput=0),
     conf.type = "log-log"))
summary( survfit(strike.cox, newdata = data.frame(uoutput=0)) )
https://cran.r-project.org/web/packages/MASS/refman/MASS.html#ucv
ucv(duration)
[1] 23.4219
https://cran.r-project.org/web/packages/MASS/refman/MASS.html#width.SJ
width.SJ(duration, method = "dpi")
[1] 38.86567