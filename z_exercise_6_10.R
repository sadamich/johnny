### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 10 (p. 527) Teleworking

xr610<- read.csv("xr610.csv", header=TRUE)
str(xr610)
attach(xr610)

### Problem (a) The Logit (H1) model: the hit rate 
eq_logit<- glm(formula = TELEWORKING ~ GENDER, 
family = binomial)
summary(eq_logit)
Call:
glm(formula = TELEWORKING ~ GENDER, family = binomial)
Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   0.5754     0.2946   1.953 0.050837 .  
GENDER       -1.4227     0.4267  -3.334 0.000855 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 138.27  on 99  degrees of freedom
Residual deviance: 126.43  on 98  degrees of freedom
AIC: 130.43
Number of Fisher Scoring iterations: 4
library(maxLik)
logLik(eq_logit)
'log Lik.' -63.21412 (df=2)

fit_probit<- fitted(eq_logit)
### Problem (b) The Logit (H0) model + additional variabe: the hit rate
eq_logit2<- glm(formula = TELEWORKING ~ GENDER+DISTANCE, 
family = binomial)
summary(eq_logit2)
Call:
glm(formula = TELEWORKING ~ GENDER + DISTANCE, family = binomial)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept) -0.71920    0.50167  -1.434  0.15169   
GENDER      -0.58821    0.50440  -1.166  0.24355### GENDER is not significant
DISTANCE     0.05136    0.01868   2.749  0.00598 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 138.27  on 99  degrees of freedom
Residual deviance: 113.59  on 97  degrees of freedom
AIC: 119.59
Number of Fisher Scoring iterations: 5
logLik(eq_logit2)
'log Lik.' -56.7966 (df=3)
### Problem (c) The reason for the difference

### Problem (d) The LR test: H0 model vs H1 model
 2*(-56.7966)-2*(-63.21412)
[1] 12.83504
1-pchisq(12.83504,1)
[1] 0.0003401882
### Problem (e) The distance preference of females and of males
plot(TELEWORKING, DISTANCE)
plot(GENDER, DISTANCE)
tele_f<- TELEWORKING[GENDER==1]
gender_f<- GENDER[GENDER==1]
distance_f<- DISTANCE[GENDER==1]
plot(gender_f, distance_f)

detach(xr610)
