### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 3 3 Bank wage (p.148)                                          ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
regression1<- lm(LOGSAL ~ 1)
summary(regression1)
### Regression 1 (p.149) Call:lm(formula = LOGSAL ~ 1)                     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.69220 -0.27098 -0.08606  0.16018  1.45624 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 10.35679    0.01825   567.5   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3973 on 473 degrees of freedom

regression2<- lm(EDUC ~ 1)
summary(regression2)
### Regression 2 (p.149) Call:lm(formula = EDUC ~ 1)                       ###
Residuals:
   Min     1Q Median     3Q    Max 
-5.492 -1.492 -1.492  1.508  7.508 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  13.4916     0.1325   101.8   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 2.885 on 473 degrees of freedom

regression3 <- lm(LOGSALBEGIN ~ 1)
summary(regression3)
### Regression 3 (p.149) Call:lm(formula = LOGSALBEGIN ~ 1)                ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.56442 -0.23693 -0.05360  0.09998  1.62013 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.66940    0.01621   596.6   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3528 on 473 degrees of freedom

dmlogsal<- resid(regression1)
dmeduc<- resid(regression2)
dmlogsalbegin<- resid(regression3)
regression4<- lm(dmlogsal ~ dmeduc + dmlogsalbegin -1)
summary(regression4)
### regression4(p.149)Call:lm(formula = dmlogsal ~ dmeduc + dmlogsalbegin - 1)###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.45035 -0.11750 -0.01215  0.11453  0.90229 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
dmeduc         0.02312    0.00389   5.945 5.39e-09 ***
dmlogsalbegin  0.86850    0.03180  27.311  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1776 on 472 degrees of freedom
Multiple R-squared:  0.8006,    Adjusted R-squared:  0.7997 
F-statistic: 947.4 on 2 and 472 DF,  p-value: < 2.2e-16

regression5<- lm(LOGSAL ~ LOGSALBEGIN)
summary(regression5)
### Regression 5 (p.149) Call:lm(formula = LOGSAL ~ LOGSALBEGIN)           ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.46515 -0.12758 -0.01248  0.10985  0.93779 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.7054     0.2322   3.038  0.00251 ** 
LOGSALBEGIN   0.9981     0.0240  41.593  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1842 on 472 degrees of freedom
Multiple R-squared:  0.7856,    Adjusted R-squared:  0.7852 
F-statistic:  1730 on 1 and 472 DF,  p-value: < 2.2e-16

regression6<- lm(EDUC ~ LOGSALBEGIN)
summary(regression6)
### Regression 6 (p.149) Call:lm(formula = EDUC ~ LOGSALBEGIN)             ###
Residuals:
    Min      1Q  Median      3Q     Max 
-6.4421 -1.0776  0.4218  1.5354  4.7201 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -40.7197     2.6504  -15.36   <2e-16 ***
LOGSALBEGIN   5.6065     0.2739   20.47   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 2.102 on 472 degrees of freedom
Multiple R-squared:  0.4702,    Adjusted R-squared:  0.4691 
F-statistic: 418.9 on 1 and 472 DF,  p-value: < 2.2e-16

res_logsal<- resid(regression5)
res_educ<- resid(regression6)
regression7<- lm(res_logsal ~ res_educ -1)
summary(regression7)
### Regression 7 (p.149) Call:lm(formula = res_logsal ~ res_educ - 1)      ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.45035 -0.11750 -0.01215  0.11453  0.90229 
Coefficients:
         Estimate Std. Error t value Pr(>|t|)    
res_educ 0.023122   0.003885   5.951 5.19e-09 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1774 on 473 degrees of freedom
Multiple R-squared:  0.06966,   Adjusted R-squared:  0.06769 
F-statistic: 35.42 on 1 and 473 DF,  p-value: 5.192e-09
### Exhibit 3 11 The partial regression scatter (p.150)                    ###
plot(res_educ,res_logsal)
fit<- fitted(regression7)




