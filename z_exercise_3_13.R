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
eqjob3<- lm(LOGSAL ~ EDUC)
summary(eqjob3)
### Regression ###
Call:
lm(formula = LOGSAL ~ EDUC)
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

### Regression ###
eqjob3a<- lm(LOGSAL ~ EDUC+LOGSALBEGIN)
summary(eqjob3a)
### Regression  Call:###
lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.37715 -0.12583 -0.03243  0.14099  0.57951 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.92458    0.77179   6.381 1.03e-08 ***
EDUC         0.02535    0.01527   1.661    0.101    
LOGSALBEGIN  0.55174    0.08384   6.581 4.29e-09 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1972 on 81 degrees of freedom
Multiple R-squared:  0.474,     Adjusted R-squared:  0.461 
F-statistic:  36.5 on 2 and 81 DF,  p-value: 5.001e-12

### Regression   ###
eqjob3b<-lm(LOGSAL ~ EDUC+LOGSALBEGIN+ GENDER)
summary(eqjob3b)
### Regression Call: ###
lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN + GENDER)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.37880 -0.13100 -0.02107  0.13468  0.56971 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.19122    0.84400   6.151 2.87e-08 ***
EDUC         0.02443    0.01535   1.592    0.115    
LOGSALBEGIN  0.52217    0.09199   5.676 2.13e-07 ***
GENDER       0.06013    0.07610   0.790    0.432    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1977 on 80 degrees of freedom
Multiple R-squared:  0.4781,    Adjusted R-squared:  0.4585 
F-statistic: 24.43 on 3 and 80 DF,  p-value: 2.55e-11

### Regression  ###
eqjob3c<- lm(LOGSAL~ EDUC+LOGSALBEGIN+GENDER-MINORITY)
summary(eqjob3c)??????????
Call:
lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN + GENDER - MINORITY)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.37880 -0.13100 -0.02107  0.13468  0.56971 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.19122    0.84400   6.151 2.87e-08 ***
EDUC         0.02443    0.01535   1.592    0.115    
LOGSALBEGIN  0.52217    0.09199   5.676 2.13e-07 ***
GENDER       0.06013    0.07610   0.790    0.432    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1977 on 80 degrees of freedom
Multiple R-squared:  0.4781,    Adjusted R-squared:  0.4585 
F-statistic: 24.43 on 3 and 80 DF,  p-value: 2.55e-11

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
