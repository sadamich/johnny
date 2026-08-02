### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 6 4 Bank wages ###
xm604<- read.csv("xm604.csv", header = TRUE)
attach(xm604)
str(xm604)
detach(xm604)

https://cran.r-project.org/web/packages/AER/refman/AER.html#BankWages
library(AER)
library("mlogit")
fm_mlogit <- mlogit(JOBCAT ~ 1 | EDUC + MINORITY, data = xm604,
  subset = GENDER == "1", shape = "wide", reflevel = "1")
summary(fm_mlogit)
### Panel 1 (p.472) Call:
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

### Marginal effects 
effects(fm_mlogit, covariate = "EDUC", data = xm604)
effects(fm_mlogit, covariate = "MINORITY", data = xm604)
### Prediction 
preds(fm_mlogit)

### LM < LR < Wald tests                                                  ###
library("mlogit")
library("lmtest")
fm_mlogit_h <- mlogit(JOBCAT ~ 1 | EDUC + MINORITY, data = xm604,
  subset = GENDER == "1", shape = "wide", reflevel = "1",heterosc = TRUE)
summary(fm_mlogit_h)
### LR test
lrtest(fm_mlogit, fm_mlogit_h)
Likelihood ratio test
Model 1: JOBCAT ~ 1 | EDUC + MINORITY
Model 2: JOBCAT ~ 1 | EDUC + MINORITY
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1   6 -118.74                         
2   8 -219.26  2 201.05  < 2.2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### Wald test 
waldtest(fm_mlogit_h)
  Wald test
data:  homoscedasticity
chisq = 0.56119, df = 2, p-value = 0.7553
### LM test
scoretest(fm_mlogit, heterosc = TRUE)
 score test
data:  heterosc = TRUE
chisq = 17.749, df = 2, p-value = 0.0001399
alternative hypothesis: heteroscedastic model

### Without the explanatory variables                                      ###
fm_mlogit_c <- mlogit(JOBCAT ~ 1 , data = xm604,
  subset = GENDER == "1", shape = "wide", reflevel = "1")
summary(fm_mlogit_c)

### Panel 2 (p. 472) Call:
mlogit(formula = JOBCAT ~ 1, data = xm604, subset = GENDER == 
    "1", reflevel = "1", shape = "wide", method = "nr")
Frequencies of alternatives:choice
      1       2       3 
0.60853 0.10465 0.28682 
nr method
5 iterations, 0h:0m:0s 
g'(-H)^-1g = 1.82E-06 
successive function values within tolerance limits 
Coefficients :
              Estimate Std. Error z-value  Pr(>|z|)    
(Intercept):2 -1.76041    0.20834 -8.4496 < 2.2e-16 ***
(Intercept):3 -0.75218    0.14101 -5.3344 9.588e-08 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -231.34
McFadden R^2:  0 
Likelihood ratio test : chisq = 0 (p.value = 1)

fm_mlogit2 <- mlogit(JOBCAT ~ 1 | EDUC + MINORITY, data = xm604,
   shape = "wide", reflevel = "1")
summary(fm_mlogit2)
Call:
mlogit(formula = JOBCAT ~ 1 | EDUC + MINORITY, data = xm604, 
    reflevel = "1", shape = "wide", method = "nr")

Frequencies of alternatives:choice
       1        2        3 
0.765823 0.056962 0.177215 
nr method
8 iterations, 0h:0m:0s 
g'(-H)^-1g = 0.000175 
successive function values within tolerance limits 
Coefficients :
                Estimate Std. Error z-value  Pr(>|z|)    
(Intercept):2   2.232195   0.962219  2.3198  0.020350 *  
(Intercept):3 -28.036563   4.031870 -6.9537 3.557e-12 ***
EDUC:2         -0.455022   0.089611 -5.0778 3.819e-07 ***
EDUC:3          1.745239   0.257476  6.7783 1.216e-11 ***
MINORITY:2      1.174653   0.434734  2.7020  0.006892 ** 
MINORITY:3     -2.118933   0.759364 -2.7904  0.005264 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -165.79
McFadden R^2:  0.4812 
Likelihood ratio test : chisq = 307.55 (p.value = < 2.22e-16)


