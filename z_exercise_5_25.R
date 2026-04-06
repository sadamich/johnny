### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm501<- read.csv("xm501.csv", header =TRUE)
str(xm501)
attach(xm501)
### Problem (a) FWLS
eq<- lm(LOGSALARY~ EDUC+GENDER+MINORITY+DUMJCAT2+DUMJCAT3)
summary(eq)
res<- resid(eq)
EDUC_sq<- EDUC^2
eq_res<- lm(res^2~ DUMJCAT2+DUMJCAT3+EDUC+EDUC_sq)
summary(eq_res)
Call:
lm(formula = res^2 ~ DUMJCAT2 + DUMJCAT3 + EDUC + EDUC_sq)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.06296 -0.03182 -0.01789  0.00629  0.72350 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)  1.628e-02  5.330e-02   0.305    0.760
DUMJCAT2    -1.238e-02  1.362e-02  -0.909    0.364
DUMJCAT3     8.538e-03  1.151e-02   0.742    0.458
EDUC         5.059e-04  8.329e-03   0.061    0.952
EDUC_sq      7.239e-05  3.245e-04   0.223    0.824
Residual standard error: 0.06521 on 469 degrees of freedom
Multiple R-squared:  0.02579,   Adjusted R-squared:  0.01748 
F-statistic: 3.104 on 4 and 469 DF,  p-value: 0.01538
fit<- fitted(eq_res)
eq_fwls<- lm(LOGSALARY~ EDUC+GENDER+MINORITY+DUMJCAT2+DUMJCAT3,weights=1/fit)
summary(eq_fwls)
Call:
lm(formula = LOGSALARY ~ EDUC + GENDER + MINORITY + DUMJCAT2 + 
    DUMJCAT3, weights = 1/fit)
Weighted Residuals:
    Min      1Q  Median      3Q     Max 
-2.5005 -0.6901 -0.0787  0.6594  4.4205 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.632344   0.047967 200.811  < 2e-16 ***
EDUC         0.039311   0.003885  10.120  < 2e-16 ***
GENDER       0.181978   0.020253   8.985  < 2e-16 ***
MINORITY    -0.067395   0.020538  -3.281  0.00111 ** 
DUMJCAT2     0.178342   0.032217   5.536 5.17e-08 ***
DUMJCAT3     0.559036   0.032881  17.002  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.002 on 468 degrees of freedom
Multiple R-squared:  0.7203,    Adjusted R-squared:  0.7173 
F-statistic:   241 on 5 and 468 DF,  p-value: < 2.2e-16
EDUC_s<- sort(EDUC)
hist(EDUC_s)
eq1<- lm(LOGSALARY[EDUC<14]~EDUC[EDUC<14]+GENDER[EDUC<14]+MINORITY[EDUC<14]
              +DUMJCAT2[EDUC<14]+DUMJCAT3[EDUC<14])
summary(eq1)
Call:
lm(formula = LOGSALARY[EDUC < 14] ~ EDUC[EDUC < 14] + GENDER[EDUC < 
    14] + MINORITY[EDUC < 14] + DUMJCAT2[EDUC < 14] + DUMJCAT3[EDUC < 
    14])
Residuals:
     Min       1Q   Median       3Q      Max 
-0.39429 -0.09832  0.00000  0.09254  0.52938 
Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)          9.766853   0.075173 129.926  < 2e-16 ***
EDUC[EDUC < 14]      0.026684   0.006587   4.051 6.92e-05 ***
GENDER[EDUC < 14]    0.172143   0.025703   6.697 1.53e-10 ***
MINORITY[EDUC < 14] -0.069209   0.024714  -2.800  0.00552 ** 
DUMJCAT2[EDUC < 14]  0.172763   0.039865   4.334 2.17e-05 ***
DUMJCAT3[EDUC < 14]  0.802059   0.166775   4.809 2.69e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1646 on 237 degrees of freedom
Multiple R-squared:  0.3798,    Adjusted R-squared:  0.3668 
F-statistic: 29.03 on 5 and 237 DF,  p-value: < 2.2e-16
eq2<- lm(LOGSALARY[EDUC>=14]~EDUC[EDUC>=14]+GENDER[EDUC>=14]+MINORITY[EDUC>=14]
              +DUMJCAT2[EDUC>=14]+DUMJCAT3[EDUC>=14])
summary(eq2)
Call:
lm(formula = LOGSALARY[EDUC >= 14] ~ EDUC[EDUC >= 14] + GENDER[EDUC >= 
    14] + MINORITY[EDUC >= 14] + DUMJCAT2[EDUC >= 14] + DUMJCAT3[EDUC >= 
    14])
Residuals:
    Min      1Q  Median      3Q     Max 
-0.4460 -0.1548 -0.0219  0.1276  0.8773 
Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)           9.00332    0.20763  43.362  < 2e-16 ***
EDUC[EDUC >= 14]      0.08290    0.01363   6.082 5.04e-09 ***
GENDER[EDUC >= 14]    0.16555    0.03402   4.867 2.14e-06 ***
MINORITY[EDUC >= 14] -0.08285    0.04004  -2.069   0.0396 *  
DUMJCAT2[EDUC >= 14] -0.23135    0.22032  -1.050   0.2948    
DUMJCAT3[EDUC >= 14]  0.44880    0.04246  10.571  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2172 on 225 degrees of freedom
Multiple R-squared:  0.7211,    Adjusted R-squared:  0.7149 
F-statistic: 116.3 on 5 and 225 DF,  p-value: < 2.2e-16
anova(eq1,eq2)


Analysis of Variance Table
Response: LOGSALARY[EDUC < 14]
                     Df Sum Sq Mean Sq F value    Pr(>F)    
EDUC[EDUC < 14]       1 0.1571 0.15712  5.7985   0.01680 *  
GENDER[EDUC < 14]     1 2.5873 2.58727 95.4859 < 2.2e-16 ***
MINORITY[EDUC < 14]   1 0.1269 0.12693  4.6844   0.03144 *  
DUMJCAT2[EDUC < 14]   1 0.4353 0.43531 16.0655 8.199e-05 ***
DUMJCAT3[EDUC < 14]   1 0.6267 0.62669 23.1286 2.695e-06 ***
Residuals           237 6.4217 0.02710                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"LOGSALARY[EDUC >= 14]"’ removed because 
  response differs from model 1

### Problem (c) Breusch Pagan test for heteroskedasticity                  ###
LM= n*R^2= 474*0.02579 = 12.22446
library(lmtest)
bptest(LOGSALARY ~ EDUC + GENDER + MINORITY + DUMJCAT2 + 
    DUMJCAT3)
studentized Breusch-Pagan test
data:  LOGSALARY ~ EDUC + GENDER + MINORITY + DUMJCAT2 + DUMJCAT3
BP = 13.04, df = 5, p-value = 0.023
### H0 (Homoskedasticity) is rejected                                      ###
### Problem (d) White test
eq_white<- lm(res^2~ DUMJCAT2+DUMJCAT3+EDUC+EDUC_sq+GENDER+MINORITY)
summary(eq_white)
474* 0.02761 = 13.08714
