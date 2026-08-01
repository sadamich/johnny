### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 13 (p. 528)  Bank wages

xr613<- read.csv("xr613.csv", header=TRUE)
str(xr613)
attach(xr613)
j<- ifelse(JOBCAT==3, 1, 0)
### Problem (a) Logit model
eq_logit_j<- glm(formula = j~ EDUC+ MINORITY+PREVEXP, 
family = binomial)
summary(eq_logit_j)

eq_logit<- glm(formula = Y~ EDUC+ MINORITY+PREVEXP, 
family = binomial)
summary(eq_logit)
Call: glm(formula = Y ~ EDUC + MINORITY + PREVEXP, family = binomial)
Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -26.952526   4.400955  -6.124 9.11e-10 ***
EDUC          1.674803   0.280053   5.980 2.23e-09 ***
MINORITY     -2.395242   0.847987  -2.825  0.00473 ** 
PREVEXP       0.003865   0.003078   1.256  0.20919    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 309.23  on 257  degrees of freedom
Residual deviance: 126.78  on 254  degrees of freedom
AIC: 134.78
Number of Fisher Scoring iterations: 8
library(maxLik)
logLik(eq_logit)
'log Lik.' -63.39219 (df=4)
### Problem (b) Marginal effects
eq_logit2<- glm(formula = Y~ EDUC+PREVEXP, 
family = binomial)
summary(eq_logit2)
Call: glm(formula = Y ~ EDUC + PREVEXP, family = binomial)
Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.506e+01  3.874e+00  -6.468 9.91e-11 ***
EDUC         1.548e+00  2.457e-01   6.299 2.99e-10 ***
PREVEXP      8.677e-04  2.884e-03   0.301    0.764    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 309.23  on 257  degrees of freedom
Residual deviance: 137.92  on 255  degrees of freedom
AIC: 143.92
Number of Fisher Scoring iterations: 7
logLik(eq_logit2)
'log Lik.' -68.95922 (df=3)
2*(-63.39219) - 2*(-68.95922)
[1] 11.13406
1- pchisq(11.13406,1)
[1] 0.0008475687   (H0 is rejected) 
-2.395242*the error correction facter (logit)

### Problem (c) LR test: H0 model vs H1 Model: the heteroskedasticity
eq_logit3<- glm(formula = Y~ EDUC+ MINORITY, 
family = binomial)
summary(eq_logit3)
Call:glm(formula = Y ~ EDUC + MINORITY, family = binomial)
Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -26.2147     4.3117  -6.080 1.20e-09 ***
EDUC          1.6448     0.2767   5.944 2.78e-09 ***
MINORITY     -2.1197     0.7940  -2.670  0.00759 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 309.23  on 257  degrees of freedom
Residual deviance: 128.30  on 255  degrees of freedom
AIC: 134.3
Number of Fisher Scoring iterations: 8
logLik(eq_logit3)
'log Lik.' -64.15011 (df=3)
2*(-63.39219)- 2*(-64.15011)
[1] 1.51584
1-pchisq(1.51584,1)
[1] 0.2182501     (H0 is not rejected) 
### Problem (d) McFadden R_sq: hit rate

### Problem (e) Compare with binary logit model and the multinomial logit model
mlogit(formula = JOBCAT ~ 1 | EDUC + MINORITY, data = xm604, 
    subset = GENDER == "1", reflevel = "1", shape = "wide", method = "nr")
Frequencies of alternatives:choice
      1       2       3 
0.60853 0.10465 0.28682 
nr method
8 iterations, 0h:0m:0s 
g'(-H)^-1g = 9.15E-06 
successive function values within tolerance limits 
Coefficients :
                Estimate Std. Error z-value  Pr(>|z|)    
(Intercept):2   4.760722   1.172774  4.0594 4.921e-05 ***
(Intercept):3 -26.014104   4.314443 -6.0295 1.644e-09 ***
EDUC:2         -0.553399   0.099041 -5.5876 2.303e-08 ***
EDUC:3          1.633370   0.276848  5.8999 3.638e-09 ***
MINORITY:2      0.426952   0.502708  0.8493  0.395712    
MINORITY:3     -2.109089   0.794193 -2.6556  0.007916 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -118.74
McFadden R^2:  0.48676 
Likelihood ratio test : chisq = 225.22 (p.value = < 2.22e-16)





### Problem (f) Logit, multinomial, ordered Logit model


detach(xr613)
