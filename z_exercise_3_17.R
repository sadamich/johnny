### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr317<- read.csv("xr317.csv", header =TRUE)
str(xr317)
attach(xr317)
eq_adv<- lm(log(QUANTITY) ~ log(DEAL) + A)
summary(eq_adv)
### The Adv A is significant Call:lm(formula=log(QUANTITY) ~ log(DEAL)+ A) ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.12421 -0.07053 -0.03182  0.05242  0.23452 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.82465    0.03630 160.458  < 2e-16 ***
log(DEAL)    5.02723    0.44980  11.177 1.13e-08 ***
A            0.34636    0.05526   6.267 1.51e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1021 on 15 degrees of freedom
Multiple R-squared:  0.9446,    Adjusted R-squared:  0.9372 
F-statistic: 127.8 on 2 and 15 DF,  p-value: 3.79e-10
F-statistic: 81.06 on 2 and 15 DF,  p-value: 9.091e-09
eq<- lm(log(QUANTITY) ~ log(DEAL))
summary(eq)
### The model withour A Call:lm(formula = log(QUANTITY) ~ log(DEAL))        ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.284958 -0.137104 -0.002173  0.178219  0.228421 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.87212    0.06539  89.803  < 2e-16 ***
log(DEAL)    6.10882    0.76508   7.985 5.69e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.188 on 16 degrees of freedom
Multiple R-squared:  0.7994,    Adjusted R-squared:  0.7868 
F-statistic: 63.75 on 1 and 16 DF,  p-value: 5.686e-07
anova(eq_adv, eq)
Analysis of Variance Table
Model 1: log(QUANTITY) ~ log(DEAL) + A
Model 2: log(QUANTITY) ~ log(DEAL)
  Res.Df     RSS Df Sum of Sq     F    Pr(>F)    
1     15 0.15627                                 
2     16 0.56548 -1  -0.40921 39.28 1.508e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### F test also says A is significant.                                          ###
### Problem (b)                                                            ###
b2= 1
(5.02723 - 1)/0.44980 = [1] 8.953379   ### H0 is rejectet.                 ###
confint(eq_adv)
        2.5 %    97.5 %
(Intercept) 5.7472780 5.9020222
log(DEAL)   4.0685024 5.9859586
A           0.2285653 0.4641463

eq_a<- lm(log(QUANTITY[A==1]) ~ log(DEAL[A==1]))
summary(eq_a)
### The Adv Moldel Call:lm(formula=log(QUANTITY[A == 1])~log(DEAL[A == 1])) ###
Residuals:
        1         2         3         4         5         6 
 0.034220 -0.032873 -0.001347 -0.015779  0.005940  0.009839 
Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        6.06341    0.02411  251.48 1.50e-09 ***
log(DEAL[A == 1])  6.16847    0.23034   26.78 1.16e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.02566 on 4 degrees of freedom
Multiple R-squared:  0.9945,    Adjusted R-squared:  0.9931 
F-statistic: 717.2 on 1 and 4 DF,  p-value: 1.156e-05
eq_no<- lm(log(QUANTITY[A==0]) ~ log(DEAL[A==0]))
summary(eq_no)
### no Adv model Call:lm(formula=log(QUANTITY[A == 0])~log(DEAL[A == 0]))  ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.14130 -0.05791 -0.04034  0.03262  0.23512 
Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        5.84174    0.04307 135.628  < 2e-16 ***
log(DEAL[A == 0])  4.66469    0.58192   8.016 1.16e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.115 on 10 degrees of freedom
Multiple R-squared:  0.8653,    Adjusted R-squared:  0.8519 
F-statistic: 64.26 on 1 and 10 DF,  p-value: 1.157e-05
anova(eq_a, eq_no)
Analysis of Variance Table
Response: log(QUANTITY[A == 1])
                  Df  Sum Sq Mean Sq F value    Pr(>F)    
log(DEAL[A == 1])  1 0.47235 0.47235  717.17 1.156e-05 ***
Residuals          4 0.00263 0.00066                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"log(QUANTITY[A == 0])"’ removed because 
response differs from model1 (The two models are diffent models.No predictable)