### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 13 (p. 528)  Bank wages

xr613<- read.csv("xr613.csv", header=TRUE)
str(xr613)
attach(xr613)


### Problem (a) Logit model
eq_logit<- glm(formula = Y~ EDUC+ MINORITY+PREVEXP, 
family = binomial)
summary(eq_logit)
Call:
glm(formula = Y ~ EDUC + MINORITY + PREVEXP, family = binomial)
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

### Problem (c) LR test: H0 model vs H1 Model: the heteroskedasticity
eq_logit2<- glm(formula = Y~ EDUC+ MINORITY, 
family = binomial)
summary(eq_logit2)
Call:
glm(formula = Y ~ EDUC + MINORITY, family = binomial)

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
logLik(eq_logit2)
'log Lik.' -64.15011 (df=3)
2*(-63.39219)- 2*(-64.15011)
[1] 1.51584
1-pchisq(1.51584,1)
[1] 0.2182501     (H0 is not rejected) 
### Problem (d) McFadden R_sq: hit rate

### Problem (e) Compare with binary logit model and the multinomial logit model

### Problem (f) Logit, multinomial, ordered Logit model


detach(xr613)
