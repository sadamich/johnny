### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
xm402<- read.csv("xm402.csv",header =TRUE)
str(xm402)
attach(xm402)
library(maxLik)
library(gslnls)
eq1<- lm(LOGQ1~LOGD1)
summary(eq1)
### Panel 1 (p.220)OLS lm(formula = LOGQ1 ~ LOGD1)                         ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.14130 -0.05791 -0.04034  0.03262  0.23512 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.84174    0.04307 135.628  < 2e-16 ***
LOGD1        4.66469    0.58192   8.016 1.16e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.115 on 10 degrees of freedom
  (12 observations deleted due to missingness)
Multiple R-squared:  0.8653,    Adjusted R-squared:  0.8519 
F-statistic: 64.26 on 1 and 10 DF,  p-value: 1.157e-05
logLik(eq1)
log Lik.' 10.01699 (df=3)
res1<- resid(eq1)
ssr1<- sum(res1^2)
LOGD1_sq<- LOGD1[1:12]^2
eq5<- lm(res1~ LOGD1[1:12]+ LOGD1_sq)
summary(eq5)
### Auxiliary regression Panel 5 (p.220)                                   ###                
### lm(formula = res1 ~ LOGD1[1:12] + LOGD1_sq)                            ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.147432 -0.043529 -0.009504  0.031315  0.171768 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.03462    0.04015  -0.862   0.4109  
LOGD1[1:12]   4.44957    2.11581   2.103   0.0648 .
LOGD1_sq    -31.96557   14.77373  -2.164   0.0587 .
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.09835 on 9 degrees of freedom
Multiple R-squared:  0.3422,    Adjusted R-squared:  0.196 
F-statistic: 2.341 on 2 and 9 DF,  p-value: 0.1519
### LM test
12*  0.3422
[1] 4.1064
1- pchisq(4.1064,1)
[1] 0.04272121      (P value) 
### H0 is rejected

eq3<- lm(LOGQ2~LOGD2)
summary(eq3)
### Panel 3 OLS lm(formula = LOGQ2 ~ LOGD2)                                ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.18705 -0.06228 -0.01716  0.05636  0.23501 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.40656    0.04305  102.36  < 2e-16 ***
LOGD2        6.00330    0.58160   10.32 1.19e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.115 on 10 degrees of freedom
  (12 observations deleted due to missingness)
Multiple R-squared:  0.9142,    Adjusted R-squared:  0.9056 
F-statistic: 106.5 on 1 and 10 DF,  p-value: 1.188e-06
logLik(eq3)
res3<- resid(eq3)
log Lik.' 10.02358 (df=3)
LOGD2_sq<- LOGD2[13:24]^2
eq6<- lm(res3 ~ LOGD2[13:24]+LOGD2_sq)
summary(eq6)
### Auxiliary regression Panel 6 (p.220)                                    ###
lm(formula = res3 ~ LOGD2[13:24] + LOGD2_sq)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.16285 -0.03896  0.01360  0.05890  0.14665 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)   -0.02876    0.04324  -0.665    0.523
LOGD2[13:24]   3.69584    2.27844   1.622    0.139
LOGD2_sq     -26.55080   15.90927  -1.669    0.129
Residual standard error: 0.1059 on 9 degrees of freedom
Multiple R-squared: 0.2363, Adjusted R-squared:  0.06663 
F-statistic: 1.393 on 2 and 9 DF,  p-value: 0.2972
### LM test
12* 0.2363
[1] 2.8356
1 - pchisq(2.8356,1)
[1] 0.09219636
### H0 is not rejected

### F test

F<- function(ssr_r,ssr, g, n, k){
(ssr_r - ssr)/g / (ssr/(n-k))
}
F(0.1323,0.087,1,12,3)
[1] 4.686207
1- pf(4.686207, 1, 9)
[1] 0.05861102
### H0 is not rejected

F(0.1322, 0.1009,1,12,3)
[1] 2.791873
### H0 is not rejected

1 - pf(2.791873, 1,9)
[1] 0.1290795


### LR test (p.247)

2*(12.530 - 10.017)
[1] 5.026
1 - pchisq(5.026,1)
[1] 0.0249695       (P value)

2*(11.641 - 10.024)
[1] 3.234
1 - pchisq(3.234,1)
[1] 0.07212432      (P value)

### W test   n/(n-k)*t^2
pt(-2.012,9)*2
[1] 0.07508534      (P value)

pt(-1.651,9)*2
[1] 0.1331341       (P value) 

W<- function(t,n,k){
(n/(n-k))*(t^2)
}
W(-2.012,12,3)
[1] 5.397525
1- pchisq( 5.397525,1)
[1] 0.02016533

W(-1.651,12,3)
[1] 3.634401
1 - pchisq(3.634401,1)
[1] 0.05659697      (P value)