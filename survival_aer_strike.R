library(AER)
data("StrikeDuration")
str(StrikeDuration)
attach(StrikeDuration)
library("MASS")

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