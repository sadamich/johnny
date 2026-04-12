### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm501<- read.csv("xm501.csv", header =TRUE)
str(xm501)
attach(xm501)
xm501_o<- xm501[order(xm501$EDUC), ]
detach(xm501)
attach(xm501_o)
library(strucchange)
eq_break<- lm(LOGSALARY ~ EDUC+GENDER+MINORITY)
eq_cusum<- efp(LOGSALARY ~ EDUC+GENDER+MINORITY,type = "Rec-CUSUM")
plot(eq_cusum)
sctest(eq_cusum)

eq<- lm(LOGSALARY~ EDUC+GENDER+MINORITY)
res<- resid(eq)
sum(res^2)
### Problem (b)the stability of the parameter                              ###                         
eq1<- lm(LOGSALARY[EDUC<16]~ EDUC[EDUC<16]
                            +GENDER[EDUC<16]+MINORITY[EDUC<16])
summary(eq1)
res1<- resid(eq1)
sum(res1^2)
### The Subset EDUC < 16                                                   ###
### lm(formula = LOGSALARY[EDUC < 16] ~ EDUC[EDUC < 16] +                  ###
### GENDER[EDUC < 16] + MINORITY[EDUC < 16])                               ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.43952 -0.11930 -0.01443  0.11169  0.88380 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)          9.761619   0.055637 175.453  < 2e-16 ***
EDUC[EDUC < 16]      0.027696   0.004495   6.162 1.92e-09 ***
GENDER[EDUC < 16]    0.228921   0.020448  11.195  < 2e-16 ***
MINORITY[EDUC < 16] -0.069008   0.022853  -3.020  0.00271 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1875 on 361 degrees of freedom
Multiple R-squared:  0.3714,    Adjusted R-squared:  0.3662 
F-statistic: 71.09 on 3 and 361 DF,  p-value: < 2.2e-16

eq2<- lm(LOGSALARY[EDUC>=16]~ EDUC[EDUC>=16]
                            +GENDER[EDUC>=16]+MINORITY[EDUC>=16])
summary(eq2)
res2<- resid(eq2)
sum(res2^2)
### The Subset of EDCU >=16                                                ###
### lm(formula = LOGSALARY[EDUC >= 16] ~ EDUC[EDUC >= 16] +                ###
### GENDER[EDUC >= 16] + MINORITY[EDUC >= 16])                             ###

Residuals:
     Min       1Q   Median       3Q      Max 
-0.66040 -0.17269 -0.03612  0.13514  0.91430 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)           9.32281    0.34448  27.063  < 2e-16 ***
EDUC[EDUC >= 16]      0.07838    0.02114   3.708 0.000335 ***
GENDER[EDUC >= 16]    0.33957    0.06910   4.914  3.3e-06 ***
MINORITY[EDUC >= 16] -0.31790    0.08439  -3.767 0.000273 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2721 on 105 degrees of freedom
Multiple R-squared:  0.4414,    Adjusted R-squared:  0.4255 
F-statistic: 27.66 on 3 and 105 DF,  p-value: 2.915e-13
anova(eq,eq2)
 Table
Response: LOGSALARY[EDUC < 16]
                     Df  Sum Sq Mean Sq F value    Pr(>F)    
EDUC[EDUC < 16]       1  3.0192  3.0192  85.842 < 2.2e-16 ***
GENDER[EDUC < 16]     1  4.1617  4.1617 118.323 < 2.2e-16 ***
MINORITY[EDUC < 16]   1  0.3207  0.3207   9.118  0.002711 ** 
Residuals           361 12.6972  0.0352                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"LOGSALARY[EDUC >= 16]"’ removed because 
  response differs from model 

### Problem c (p.433)                                                      ###
F_break<- function(ssr, ssr2,ssr3,k,n2,n3){
result<- ((ssr - ssr2 -ssr3)/k) / ((ssr2+ssr3)/(n2+n3 - 2*k))
return(result)
}
F_break(30.85177,12.69721,7.772112,4,365,109)
[1] 59.09112

F_forecast<- function(ssr, ssr2,k,n2,n3){
result<- ((ssr - ssr2)/n3) / (ssr2/(n2-k))
return(result)
}
F_forecast(30.85177,12.69721 ,4,365,109)
[1] 4.735416