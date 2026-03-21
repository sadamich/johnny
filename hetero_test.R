### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm501<- read.csv("xm501.csv", header=TRUE)
attach(xm501)
str(xm501)
panel01<- lm(LOGSALARY ~ EDUC+GENDER+MINORITY+DUMJCAT2+DUMJCAT3)
summary(panel01)
panel02<- lm(LOGSALARY[JOBCAT==1] ~ EDUC[JOBCAT==1]+GENDER[JOBCAT==1]
         +MINORITY[JOBCAT==1])
summary(panel02)
anova(panel01,panel02)
panel03<- lm(LOGSALARY[JOBCAT==2] ~ EDUC[JOBCAT==2]+GENDER[JOBCAT==2]
         +MINORITY[JOBCAT==2])
summary(panel03)
anova(panel01, panel03)
panel04<- lm(LOGSALARY[JOBCAT==3] ~ EDUC[JOBCAT==3]+GENDER[JOBCAT==3]
         +MINORITY[JOBCAT==3])
summary(panel04)
anova(panel01, panel04)

xm513<- read.csv("xm513.csv", header =TRUE)
str(xm513)
attach(xm513)
panel01<- lm(MEANLOGSAL~ MEANEDUC+GENDER+MINORITY+DUMJCAT2+DUMJCAT3)
summary(panel01)
res1<- resid(panel01)
res1_sq<- res1^2
EDUC_sq<- MEANEDUC^2 
panel02<- lm(res1_sq~ MEANEDUC+EDUC_sq+GENDER+MINORITY+DUMJCAT2+DUMJCAT3)
summary(panel02)
### Panel 2 White heteroskedasticity test (p.348) Call:                    ###
###  lm(formula = res1_sq ~ MEANEDUC + EDUC_sq + GENDER + MINORITY +       ###
    DUMJCAT2 + DUMJCAT3)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.049963 -0.015243 -0.002792  0.006649  0.116020 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept) -3.706e-02  1.124e-01  -0.330    0.745
MEANEDUC     2.928e-03  1.841e-02   0.159    0.875
EDUC_sq     -1.302e-05  7.201e-04  -0.018    0.986
GENDER       9.429e-03  1.853e-02   0.509    0.617
MINORITY     9.845e-03  1.560e-02   0.631    0.536
DUMJCAT2     1.993e-02  2.244e-02   0.888    0.385
DUMJCAT3     1.931e-02  2.077e-02   0.930    0.364
Residual standard error: 0.0385 on 19 degrees of freedom
Multiple R-squared:  0.2096,    Adjusted R-squared:  -0.04005 
F-statistic: 0.8396 on 6 and 19 DF,  p-value: 0.5548 (H0 is not rejectet)
LM = 26* 0.2096 = [1] 5.4496 (P = 0.48)


group<- 1/GROUPSIZE
panel03<- lm(res1_sq~ group)
summary(panel03)
### Panel 3 Breusch Pagan test (p.349) Call:                               ###
lm(formula = res1_sq ~ group)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.062101 -0.011866 -0.002254  0.010141  0.098744 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) 0.002398   0.008236   0.291  0.77339   
group       0.059922   0.018879   3.174  0.00409 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.03234 on 24 degrees of freedom
Multiple R-squared:  0.2956,    Adjusted R-squared:  0.2663 
F-statistic: 10.07 on 1 and 24 DF,  p-value: 0.00409
LM= 26*0.2956 = [1] 7.6856 (P= 0.006) H0 is rejectet. 

### Example 5 18 Interest and Bond rates ###
xm511<- read.csv("xm511.csv", header = TRUE)
str(xm511)
attach(xm511)
panel01<- lm(DAAA~ DUS3MT)
summary(panel01)
res<- resid(panel01)
DUS3MT_sq<- DUS3MT^2
panel02<- lm(res^2 ~ DUS3MT+ DUS3MT_sq)
summary(panel02)
### Panel 2 The White test (p.350) lm(formula = res^2 ~ DUS3MT + DUS3MT_sq)###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.10722 -0.02727 -0.02409 -0.00750  1.11221 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.0276542  0.0032463   8.519   <2e-16 ***
DUS3MT      -0.0002238  0.0070729  -0.032   0.9748    
DUS3MT_sq    0.0065596  0.0028043   2.339   0.0197 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07788 on 597 degrees of freedom
Multiple R-squared:  0.0103,    Adjusted R-squared:  0.006984 
F-statistic: 3.106 on 2 and 597 DF,  p-value: 0.04549 (H0 is rejectet) 
LM = 600* 0.0103 = [1] 6.18 (H0 is rejectet) 

daaa_res<- resid(panel01)
standard_res<- ts(daaa_res/0.1710)
### Exhibit 5 27 (a) p.351 ###
plot(standard_res)

panel01<- lm(DAAA~ DUS3MT)
summary(panel01)
eq_aux<- lm(daaa_res^2~ DUM7599)
summary(eq_aux)
res_aux<- resid(eq_aux)
fit2<- fitted(eq_aux)
standard_res2<- ts(daaa_res/fit2)
plot(standard_res2)

