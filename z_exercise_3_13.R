### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 3 13 (p.183)                                                  ###
xm301<- read.csv("xm301.csv", header = TRUE)
attach(xm301)
str(xm301)
### JOBCAT Subset/Teilmenge ###
xm301JOBCAT <- subset(xm301, JOBCAT==3)
str(xm301JOBCAT)
attach(xm301JOBCAT)
eq_base<- lm(LOGSAL~EDUC+LOGSALBEGIN+GENDER+MINORITY)
summary(eq_base)
eqjob3<- lm(LOGSAL ~ EDUC)
summary(eqjob3)
### Regression ###
Call:lm(formula = LOGSAL ~ EDUC)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.49207 -0.17695 -0.01448  0.11240  0.65537 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.76756    0.28641  34.103  < 2e-16 ***
EDUC         0.07316    0.01653   4.425 2.94e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2428 on 82 degrees of freedom
Multiple R-squared:  0.1928,    Adjusted R-squared:  0.1829 
F-statistic: 19.58 on 1 and 82 DF,  p-value: 2.938e-05
anova(eq_base, eqjob3)
Analysis of Variance Table
Model 1: LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY
Model 2: LOGSAL ~ EDUC
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1     79 3.0659                                  
2     82 4.8354 -3   -1.7695 15.198 6.753e-08 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 
### H0 (b3=b4=b5=0) is rejected                                            ###
eq_const<- lm(LOGSAL~1)
summary(eq_const)
anova(eq_base, eq_const)
Analysis of Variance Table
Model 1: LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY
Model 2: LOGSAL ~ 1
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1     79 3.0659                                  
2     83 5.9903 -4   -2.9244 18.838 6.561e-11 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
### H0 (b2=b3=b4=b5=0) is rejected)                                        ###


eq_no_gm<- lm(LOGSAL~ EDUC+LOGSALBEGIN)
summary(eq_no_gm)
Analysis of Variance Table
Model 1: LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY
Model 2: LOGSAL ~ EDUC + LOGSALBEGIN
  Res.Df    RSS Df Sum of Sq      F Pr(>F)
1     79 3.0659                           
2     81 3.1507 -2 -0.084841 1.0931 0.3402
### H0 (b4=b5=0) is not rejected                                           ### 

anova(eq_base,eq_no_gm)
library(car)
linearHypothesis(eq_base, "1*GENDER + 1*MINORITY = 0")
Linear hypothesis test:
GENDER  + MINORITY = 0
Model 1: restricted model
Model 2: LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY
  Res.Df    RSS Df Sum of Sq      F Pr(>F)
1     80 3.1503                           
2     79 3.0659  1  0.084468 2.1765 0.1441
### H0 (b4+b5=0) is not rejected                                           ###

### Subset/Teilmenge JOBCAT 2 ###
xm301JOBCAT2 <- subset(xm301, JOBCAT==2)
str(xm301JOBCAT2)
attach(xm301JOBCAT2)
eqjob2<- lm(LOGSAL~ EDUC)
summary(eqjob2)
### Regression  call:###
lm(formula = LOGSAL ~ EDUC)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.217922 -0.020472 -0.000669  0.011485  0.140796 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 10.382507   0.065056 159.594   <2e-16 ***
EDUC        -0.004424   0.006246  -0.708    0.485    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07069 on 25 degrees of freedom
Multiple R-squared:  0.01967,   Adjusted R-squared:  -0.01955 
F-statistic: 0.5016 on 1 and 25 DF,  p-value: 0.4854

### Regression ###
eqjob2a<- lm(LOGSAL ~ EDUC+LOGSALBEGIN)
summary(eqjob2a)
### Call:###
lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.218832 -0.021685 -0.001883  0.015277  0.139583 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 10.208547   1.293225   7.894 3.99e-08 ***
EDUC        -0.004234   0.006526  -0.649    0.523    
LOGSALBEGIN  0.017890   0.132819   0.135    0.894    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07212 on 24 degrees of freedom
Multiple R-squared:  0.02041,   Adjusted R-squared:  -0.06122 
F-statistic:  0.25 on 2 and 24 DF,  p-value: 0.7808

### Regression ###
eqjob2b<- lm(LOGSAL ~ EDUC+LOGSALBEGIN+GENDER)
summary(eqjob2b)
