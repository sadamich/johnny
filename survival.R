### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)
str(xm609)
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

## ML estimation of exponential duration model:
library(maxLik)
t <- STRIKEDUR
loglik <- function(theta) log(theta) - theta*t
## Estimate with numeric gradient and hessian
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




