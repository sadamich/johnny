### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)
str(xm609)
'data.frame':   62 obs. of  4 variables:
 $ OBS         : int  1 2 3 4 5 6 7 8 9 10 ...
 $ STRIKEDUR   : int  1 2 2 2 3 3 3 3 3 4 ...
 $ PROD        : num  0.0645 -0.007 0.00535 0.0645 0.07427 ...
 $ STRIKECENS80: int  1 2 2 2 3 3 3 3 3 4 .
library(survival)
with(xm609, Surv(STRIKEDUR))
[1]   1   2   2   2   3   3   3   3   3   4   5   7   8   9   9  10  11  12  12
[20]  13  14  15  17  19  21  21  22  23  25  26  27  27  28  29  32  33  35  37
[39]  38  41  42  43  43  44  49  49  52  52  61  72  85  98  99 100 104 114 117
[58] 119 130 152 153 216

survfit(Surv(STRIKEDUR) ~ 1, data=xm609)
### Call: survfit(formula = Surv(STRIKEDUR) ~ 1, data = xm609)
      n events median 0.95LCL 0.95UCL
[1,] 62     62     27      21      41

### Exhibit 6 14 a and b (p.512)                                           ###
hist(STRIKEDUR)
summary(STRIKEDUR)
LOGSTRIKE<- log(STRIKEDUR)
hist(LOGSTRIKE)
summary(LOGSTRIKE)
panel03<- lm(LOGSTRIKE ~ 1)
summary(panel03)
### Panel 3 (p.517) Call: lm(formula = LOGSTRIKE ~ 1)                      ###
Residuals:
    Min      1Q  Median      3Q     Max 
-3.1045 -0.7780  0.1914  0.8319  2.2708 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   3.1045     0.1644   18.88   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.295 on 61 degrees of freedom
panel04<- lm(LOGSTRIKE~PROD)
summary(panel04)
### Panel 4 (p.517) Call:lm(formula = LOGSTRIKE ~ PROD)                    ###
Residuals:
    Min      1Q  Median      3Q     Max 
-2.6135 -0.7850  0.2115  0.9307  1.9917 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)    3.206      0.161  19.912  < 2e-16 ***
PROD          -9.181      3.404  -2.697  0.00907 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.233 on 60 degrees of freedom
Multiple R-squared:  0.1081,    Adjusted R-squared:  0.09324 
F-statistic: 7.273 on 1 and 60 DF,  p-value: 0.009073

### Panel 5 ML estimation of exponential duration model (p.518)            ###
library(maxLik)
t <- STRIKEDUR
loglik <- function(theta) log(theta) - theta*t
a <- maxLik(loglik, start=1 )
summary(a)
Maximum Likelihood estimation
Newton-Raphson maximisation, 5 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -294.7275 
1  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,] 0.023432   0.002976   7.874 3.44e-15 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


library("survival")
s<- survreg(Surv(t, t>0, type="left") ~ 1, xm609, dist='exponential')
summary(s)

Call:
survreg(formula = Surv(t, t > 0, type = "left") ~ 1, data = xm609, 
    dist = "exponential")
            Value Std. Error    z      p
(Intercept) 3.754      0.127 29.6 <2e-16
Scale fixed at 1 
Exponential distribution
Loglik(model)= -294.7   Loglik(intercept only)= -294.7 ### the same value
Number of Newton-Raphson Iterations: 5 


### Panel 6 Proportional hazard model (p.518)                              ###
loglik_pro <- function(theta) {
beta0<- theta[1]
beta1<- theta[2]
beta0*PROD + log(beta1) - exp(beta0*PROD)*beta1*t
}
b <- maxLik(loglik_pro, start= c(1,1))
summary(b)
Maximum Likelihood estimation
Newton-Raphson maximisation, 7 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -289.7647 
2  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,] 9.332934   4.502091   2.073   0.0382 *  
[2,] 0.022903   0.003122   7.335 2.21e-13 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’


s2<- survreg(Surv(t, t>0, type="left") ~ PROD, xm609, dist='exponential')
summary(s2)
Call:
survreg(formula = Surv(t, t > 0, type = "left") ~ PROD, data = xm609, 
    dist = "exponential")
             Value Std. Error     z      p
(Intercept)  3.777      0.131 28.80 <2e-16
PROD        -9.334      2.960 -3.15 0.0016
Scale fixed at 1 
Exponential distribution
Loglik(model)= -289.8   Loglik(intercept only)= -294.7  ### the same value
        Chisq= 9.93 on 1 degrees of freedom, p= 0.0016 
Number of Newton-Raphson Iterations: 4 
n= 62 

### Panel 7 Weibull Model (p.518)                                          ###
loglik_w <- function(theta) {
beta0<- theta[1]
beta1<- theta[2]
sum(log(beta0)+log(beta1)+(beta0-1)*log(t)) - sum(beta1*t^(beta0))
}
c <- maxLik(loglik_w, start= c(beta=1,beta1=1))
summary(c)
Maximum Likelihood estimation
Newton-Raphson maximisation, 12 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -294.4027 
2  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
beta   0.92469    0.09160  10.095  <2e-16 ***
beta1  0.03218    0.01300   2.476  0.0133 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### Panel 8 Propotional Weibull model (p.518)                              ###
loglik_w_pro <- function(theta) {
beta0<- theta[1]
beta1<- theta[2]
beta2<- theta[3]
sum(beta2*PROD+ log(beta0)+log(beta1)+(beta0-1)*log(t)) - sum(exp(beta2*PROD)*beta1*t^(beta0))
}
d <- maxLik(loglik_w_pro, start= c(beta=1,beta1=1,beta2=1))
summary(d)
Maximum Likelihood estimation
Newton-Raphson maximisation, 12 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -289.7617 
3  free parameters
Estimates:
      Estimate Std. error t value  Pr(> t)    
beta  1.007860   0.099508  10.128  < 2e-16 ***
beta1 0.022160   0.009622   2.303 0.021275 *  
beta2 9.405741   2.548721   3.690 0.000224 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘


install.packages("NPHazardRate")
library(NPHazardRate)
lambdahat(STRIKEDUR,)

install.packages("survival")
library(survival)
eq_suv<- coxph(Surv(log(STRIKECENS80)) ~ PROD, xm609)
summary(eq_suv)
eq_ex<- aareg(Surv(log(STRIKECENS80)) ~ PROD, xm609)
summary(eq_ex)

lfit <- aareg(Surv(time, status) ~ age + sex + ph.ecog, data=lung,
                     nmin=1)


survreg(, data, weights, subset, 
        na.action, dist="weibull", init=NULL, scale=0, 
        control,parms=NULL,model=FALSE, x=FALSE,
        y=TRUE, robust=FALSE, cluster, score=FALSE, ...)

install.packages("censReg")
library("censReg")
cf) Arne Henningsen
### Panel 12 Tobit Model (p.519)                                           ###
library("survival")
t80<- STRIKECENS80
survreg(Surv(t80) ~ 1, xm609, dist='weibull',scale=1)
????



panel_12<- censReg(log(STRIKECENS80)~ 1)
summary(panel_12)
Call: censReg(formula = log(STRIKECENS80) ~ 1)
Observations:
         Total  Left-censored     Uncensored Right-censored 
            62              1             61              0 
Coefficients:
            Estimate Std. error t value Pr(> t)    
(Intercept)  3.01981    0.15256  19.794  <2e-16 ***
logSigma     0.18264    0.09105   2.006  0.0449 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Newton-Raphson maximisation, 4 iterations
Return code 1: gradient close to zero (gradtol)
Log-likelihood: -99.25352 on 2 Df
### Panel 13 Tobit Model (p.519)                                           ###
panel_13<- censReg(log(STRIKECENS80)~ PROD)
summary(panel_13)
Call: censReg(formula = log(STRIKECENS80) ~ PROD)
Observations:
         Total  Left-censored     Uncensored Right-censored 
            62              1             61              0 
Coefficients:
            Estimate Std. error t value Pr(> t)    
(Intercept)  3.10910    0.14912  20.850  <2e-16 ***
PROD        -8.11208    3.15550  -2.571  0.0101 *  
logSigma     0.13220    0.09102   1.452  0.1464    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Newton-Raphson maximisation, 3 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-likelihood: -96.10716 on 3 Df




