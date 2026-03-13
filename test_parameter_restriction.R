### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Illustration 3 4 2 Bank wages (p.166)                                  ###


xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
panel01<- lm(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY)
summary(panel01)
### Panel 1 Call:lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN                  ###
###                                   + GENDER + MINORITY)     (p.167)     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.45572 -0.11508 -0.00516  0.10765  0.87060 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.07965    0.31480   6.606 1.07e-10 ***
EDUC         0.02327    0.00387   6.013 3.66e-09 ***
LOGSALBEGIN  0.82180    0.03603  22.808  < 2e-16 ***
GENDER       0.04816    0.01991   2.419   0.0160 *  
MINORITY    -0.04237    0.02034  -2.083   0.0378 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1766 on 469 degrees of freedom
Multiple R-squared:  0.8041,    Adjusted R-squared:  0.8024 
F-statistic: 481.3 on 4 and 469 DF,  p-value: < 2.2e-16
### H0 : b2 = b3 = b4 =b5 = 0 is rejectet(F- statistic as mentioned above) ###
eq123<- lm(LOGSAL ~ EDUC+LOGSALBEGIN)
summary(eq123)
### H0 : b4 - b5 = 0                                                       ###
### Panel 2 (p.167) Call:lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.45035 -0.11750 -0.01215  0.11453  0.90229 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.646916   0.274598   5.998 3.99e-09 ***
EDUC        0.023122   0.003894   5.938 5.59e-09 ***
LOGSALBEGIN 0.868505   0.031835  27.282  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1778 on 471 degrees of freedom
Multiple R-squared:  0.8006,    Adjusted R-squared:  0.7997 
F-statistic: 945.4 on 2 and 471 DF,  p-value: < 2.2e-16
res<- resid(eq123)
ssr<- sum(res^2)
[1] 14.89166
anova(panel01, eq123)
Analysis of Variance Table
Model 1: LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY
Model 2: LOGSAL ~ EDUC + LOGSALBEGIN
  Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
1    469 14.627                              
2    471 14.892 -2  -0.26416 4.2349 0.01504 * (H0 is rejectet.)
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
### H0 b4 = -b5                                                            ###
gm<- GENDER - MINORITY
r_eq<- lm(LOGSAL ~ EDUC+LOGSALBEGIN +gm)
summary(r_eq)
### Panel 2 (p.167) Call:lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN + gm)    ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.45600 -0.11595 -0.00485  0.10857  0.87129 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 2.065371   0.308208   6.701 5.93e-11 ***
EDUC        0.023239   0.003863   6.015 3.62e-09 ***
LOGSALBEGIN 0.823543   0.035176  23.412  < 2e-16 ***
gm          0.045340   0.015612   2.904  0.00385 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1764 on 470 degrees of freedom
Multiple R-squared:  0.8041,    Adjusted R-squared:  0.8028 
F-statistic:   643 on 3 and 470 DF,  p-value: < 2.2e-16
res<- resid(r_eq)
sum(res^2)
[1] 14.62912
anova(panel01, r_eq)
Analysis of Variance Table
Model 1: LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY
Model 2: LOGSAL ~ EDUC + LOGSALBEGIN + gm
  Res.Df    RSS Df  Sum of Sq      F Pr(>F)
1    469 14.627                            
2    470 14.629 -1 -0.0016242 0.0521 0.8196 (H0 is not rejectet.)