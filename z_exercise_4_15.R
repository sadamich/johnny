### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr414<- read.csv("xr414.csv", header=TRUE)
str(xr414)
attach(xr414)
### Assumption beta5 =0                                                    ###
h0<- lm(LOGSAL ~ EDUC+LOGSALBEGIN+GENDER)
h1<- lm(LOGSAL ~ EDUC+LOGSALBEGIN)
anova(h0,h1)
summary(h0)
res1<- resid(h1)
eq_lm<- lm(res1 ~ EDUC+LOGSALBEGIN)
summary(eq_lm)
LM= n*R^2= 474* 7.534e-32 = 3.571116e-29

### The regression without Minority:lm(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER)###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.45101 -0.11130 -0.01224  0.10796  0.88370 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.932281   0.307832   6.277 7.85e-10 ***
EDUC        0.023378   0.003883   6.021 3.50e-09 ***
LOGSALBEGIN 0.836406   0.035468  23.582  < 2e-16 ***
GENDER      0.039600   0.019551   2.025   0.0434 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1772 on 470 degrees of freedom
Multiple R-squared:  0.8023,    Adjusted R-squared:  0.801 
F-statistic: 635.8 on 3 and 470 DF,  p-value: < 2.2e-16

h0_full<- lm(LOGSAL ~ EDUC+LOGSALBEGIN+GENDER+MINORITY)
anova(h0_full, h0)
Analysis of Variance Table

Model 1: LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY
Model 2: LOGSAL ~ EDUC + LOGSALBEGIN + GENDER
  Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
1    469 14.627                              
2    470 14.763 -1   -0.1353 4.3382 0.03781 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### If a significance level at 0.025, h0 can not be rejectet. That means    ###
### beta4 (GENDER) = beta5 (MINORITY)  = 0.                                 ###