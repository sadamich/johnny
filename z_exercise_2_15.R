### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 2 15 (p.115)
xr215<- read.csv("xr215.csv",header=TRUE)
str(xr215)
attach(xr215)
### Outlier: the crash observation i = 94 (October 1987)
### Problem (a) the regression with all observations                       ###
eq<- lm(RENDCYCO ~ RENDMARK)
summary(eq)
Call:lm(formula = RENDCYCO ~ RENDMARK)
Residuals:
     Min       1Q   Median       3Q      Max 
-20.4122  -3.5274   0.2316   3.4774  15.1150 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.44748    0.36294  -1.233    0.219    
RENDMARK     1.17113    0.07539  15.535   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 5.543 on 238 degrees of freedom
Multiple R-squared:  0.5035,    Adjusted R-squared:  0.5014 
F-statistic: 241.3 on 1 and 238 DF,  p-value: < 2.2e-16
### Problem (a) the regression without outliers                            ###
eq_no<- lm(RENDCYCO[-94] ~ RENDMARK[-94])
summary(eq_no)
Call:lm(formula = RENDCYCO[-94] ~ RENDMARK[-94])
Residuals:
     Min       1Q   Median       3Q      Max 
-20.3827  -3.5609   0.2294   3.5169  15.1905 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   -0.42262    0.36710  -1.151    0.251    
RENDMARK[-94]  1.15554    0.08203  14.086   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 5.552 on 237 degrees of freedom
Multiple R-squared:  0.4557,    Adjusted R-squared:  0.4534 
F-statistic: 198.4 on 1 and 237 DF,  p-value: < 2.2e-16

### Problem (b) The comparision all observations with no outliers         ###             
### The cofficients became smaller, but s.e. larger. Then t- value smaller###
### Problem (c)                                                           ###
y_94<- -0.44748+1.17113*RENDMARK[94]
y_94
[1] -33.08651
RENDCYCO[94]
[1] -35.56618
(underestimated)

### Problem (e) 
eq_it<- lm(RENDIT~ RENDMARK)
summary(eq_it)
eq_it_no<- lm(RENDIT[-94] ~ RENDMARK[-94])
summary(eq_it_no)

### Problem (e)
eq_n<- lm(RENDNCCO ~ RENDMARK)
summary(eq_n)
eq_n_no<- lm(RENDNCCO[-94] ~ RENDMARK[-94])
summary(eq_n_no)

### Problem (e)
eq_tel<- lm(RENDTEL ~ RENDMARK)
summary(eq_tel)
eq_tel_no<- lm(RENDTEL[-94] ~ RENDMARK[-94])
summary(eq_tel_no)


### Problem (f)  RENDCYCO H0 (b = 1)
(1.17113-1)/0.07539 
[1] 2.26993         (t value)
2*(1-pt(2.26993,238))
[1] 0.02410697      (P value)  H0 is rejected.
### Problem (f)  RENDIT H0 (b=1)
(0.8227-1)/ 0.1229 
[1] -1.442636       (t value)
2*(pt(-1.442636,238))
[1] 0.1504378       (P value)  H0 is not rejected.
### Problem (f) RENNCCO H0 (b =1)
(0.93154-1)/ 0.03918 
[1] -1.74732        (t value)
2*(pt(-1.74732,238))
[1] 0.08187163      (P value)  H0 is not rejected.
### Problem (f) RENDTEL H0 (b=1)
(1.08970-1)/0.05099 
[1] 1.759168        (t value)
2*(1-pt(1.759168,238))
[1] 0.07983393      (P value)  H0 is not rejected.


### Example 2 11 (p.107)                                                  ###
xm202<- read.csv("xm202.csv",header=TRUE)
str(xm202)
attach(xm202)
salary<- SALARY[1:424]
educ<- EDUC[1:424]
eq<- lm( log(salary)~ educ)
summary(eq)
### Panel (a) (p. 109) :lm(formula = log(salary) ~ educ)                  ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.57521 -0.18317 -0.02932  0.14344  1.06716 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 9.387947   0.068722  136.61   <2e-16 ***
educ        0.068414   0.005233   13.07   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2623 on 422 degrees of freedom
Multiple R-squared:  0.2883,    Adjusted R-squared:  0.2866 
F-statistic: 170.9 on 1 and 422 DF,  p-value: < 2.2e-16
res<- resid(eq)
ssr<- sum(res^2)
s_sq<- ssr/(424-2)
s_sq
[1] 0.06878685    (the estimated variance)  

### the forecast function from estimation set                             ###
f<- 9.387947 +0.068414*EDUC(forecast set) 

f<- function(x){
result<- 9.387947 +0.068414*x
return(result)
}
f(12)
### forecast for (425:474) from the set of estimation set (1:424)         ###
f_50<- 9.387947 +0.068414*EDUC[425:474]
f_e<- log(SALARY[425:474]) - f_50
### the average squared prediction error                                  ###
sum(f_e^2/50)
[1] 0.2675874

var(f_e)

### Full sample estimation                                                ###
eq_full<- lm(log(SALARY)~EDUC)
summary(eq_full)
Call:lm(formula = log(SALARY) ~ EDUC)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.66260 -0.19303 -0.03559  0.16538  0.95223
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 9.062102   0.062738   144.4   <2e-16 ***
EDUC        0.095963   0.004548    21.1   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2853 on 472 degrees of freedom
Multiple R-squared:  0.4854,    Adjusted R-squared:  0.4844 
F-statistic: 445.3 on 1 and 472 DF,  p-value: < 2.2e-16
res_full<- resid(eq_full)
ssr_full<- sum(res_full^2)

### forecast from full estimatio set                                      ###
f_50full<- 9.062102+0.095963*EDUC[425:474]
f_efull<- log(SALARY[425:474]) - f_50full
### average squared residual                                              ###
sum(f_efull^2/50)
[1] 0.1423139
var(f_efull)

### the variance of the forecast errors (2 39)                            ###
x_1<- EDUC[1:424]
x_2<- EDUC[425:474]
0.06878685 *(1 + 1/50+ sum((x_2 - mean(EDUC))^2)/sum((x_2 - mean(EDUC))^2))
[1] 0.1389494
