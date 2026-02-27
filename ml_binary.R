### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm601<- read.csv("xm601.csv", header = TRUE)
attach(xm601)
str(xm601)
AGE_2<- AGE^2/100
panel01<- lm(RESPONSE~GENDER+ACTIVITY+AGE+AGE_2)
summary(panel01)
### OLS Panel01  (p. 451)                                                  ###
lm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2)
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.060888   0.195906  -0.311    0.756    
GENDER       0.224002   0.035809   6.256 6.06e-10 ***
ACTIVITY     0.208268   0.040669   5.121 3.70e-07 ***
AGE          0.015494   0.007861   1.971    0.049 *  
AGE_2       -0.015209   0.007507  -2.026    0.043 *  
Residual standard error: 0.4804 on 920 degrees of freedom
Multiple R-squared:  0.08154,   Adjusted R-squared:  0.07755 
F-statistic: 20.42 on 4 and 920 DF,  p-value: 3.917e-16

### Logit model panel 2 (p. 451)                                           ###
panel02<- glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, 
family = binomial)
summary(panel02)
glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, family = binomial)
Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.48836    0.88999  -2.796  0.00517 ** 
GENDER       0.95369    0.15818   6.029 1.65e-09 ***
ACTIVITY     0.91375    0.18478   4.945 7.61e-07 ***
AGE          0.06995    0.03561   1.964  0.04948 *  
AGE_2       -0.06869    0.03410  -2.015  0.04394 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 1282.1  on 924  degrees of freedom
Residual deviance: 1203.7  on 920  degrees of freedom
### Probit model (p. 451)                                                   ###
panel03<- glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, 
family = binomial(link = "probit"))
summary(panel03)
glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, family = binomial(link = "probit"))
Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.49758    0.53912  -2.778  0.00547 ** 
GENDER       0.58811    0.09661   6.088 1.15e-09 ***
ACTIVITY     0.56117    0.11162   5.027 4.97e-07 ***
AGE          0.04168    0.02159   1.930  0.05360 .  
AGE_2       -0.04098    0.02066  -1.984  0.04728 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 1282.1  on 924  degrees of freedom
Residual deviance: 1203.9  on 920  degrees of freedom
AIC: 1213.9
