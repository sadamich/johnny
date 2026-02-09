### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### 3 4 4 Illustration Bank Wages, p.174                                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
panel01<- lm(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY)
summary(panel01)
res_1<- resid(panel01)
### SSR under unrestricted model ###
sum(res_1^2)
[1] 14.6275

job2 <- subset(xm301, JOBCAT==1 | JOBCAT==3)
str(job2)
        or
job13<- subset(xm301, JOBCAT!=2)
str(job13)
attach(job13)
panel02<- lm(LOGSAL~ EDUC+LOGSALBEGIN + GENDER + MINORITY)
summary(panel02)
### Regression Call:###
lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.44926 -0.11811 -0.01078  0.10575  0.87547 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.133639   0.323277   6.600 1.18e-10 ***
EDUC         0.029102   0.004352   6.688 6.87e-11 ***
LOGSALBEGIN  0.808688   0.037313  21.673  < 2e-16 ***
GENDER       0.028500   0.020875   1.365   0.1729    
MINORITY    -0.053989   0.021518  -2.509   0.0125 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1774 on 442 degrees of freedom
Multiple R-squared:  0.8133,    Adjusted R-squared:  0.8116 
F-statistic: 481.4 on 4 and 442 DF,  p-value: < 2.2e-16
res2<- resid(panel02)
### SSR under no JOBCAT 2 ###
sum(res2^2)
[1] 13.91547
require(graphics)
new<- subset(xm301, JOBCAT==2)
attach(new)

### Forecast Error (Resisulas) ###
cat2<- lm(LOGSAL ~ EDUC+LOGSALBEGIN +GENDER+MINORITY)
fit2<- fitted(cat2)

f_2<- function(a,b,c,d,e){
result<- a+ b*EDUC +c*LOGSALBEGIN + d*GENDER + e*MINORITY
return(result)
}
fore<- f_2(2.133639,0.029102, 0.808688,0.028500,-0.053989)
fore
### Exhibit 3 19 (a) p. 176 ###
plot(LOGSAL,fore)
f_error<- LOGSAL - fore
### Exhibit 3 19 (b) p. 176 ###
hist(f_error)
summary(f_error)
 Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.22263  0.08565  0.13919  0.12856  0.17715  0.51318 
sd(f_error)
[1] 0.1269522
0.12856^2+0.126952^2
[1] 0.03264448
### The Job 2 can forecasted by Job 1 and Job 3 ###

job3<- subset(xm301, JOBCAT==1 | JOBCAT==2)
str(job3)
             or
job12<- subset(xm301, JOBCAT!=3)
str(job12)
attach(job12)
panel03<- lm(LOGSAL~ EDUC+LOGSALBEGIN + GENDER + MINORITY)
summary(panel03)
### Regression Call:###
lm(formula = LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.42113 -0.10505 -0.01393  0.09399  0.90219 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.519695   0.517151   6.806 3.86e-11 ***
EDUC         0.018640   0.003774   4.940 1.17e-06 ***
LOGSALBEGIN  0.674293   0.056446  11.946  < 2e-16 ***
GENDER       0.071522   0.020327   3.518 0.000486 ***
MINORITY    -0.040494   0.019292  -2.099 0.036465 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1616 on 385 degrees of freedom
Multiple R-squared:  0.5523,    Adjusted R-squared:  0.5477 
F-statistic: 118.7 on 4 and 385 DF,  p-value: < 2.2e-16
res3<- resid(panel03)
### SSR under no JOBCAT 3 ###
sum(res3^2)
[1] 10.05848

new2<- subset(xm301, JOBCAT==3)
attach(new2)
cat3<- lm(LOGSAL ~ EDUC+LOGSALBEGIN +GENDER+MINORITY)
f_3<- function(a,b,c,d,e){
result<- a+ b*EDUC +c*LOGSALBEGIN + d*GENDER + e*MINORITY
return(result)
}
fore3<- f_3(3.519695,0.018640,0.674293,0.071522,-0.040494)
### Exhibit 3 19 (c) p. 176 ###
plot(LOGSAL,fore3)
### Forecast Error (Residuals) ###
### Exhibit 3 19 (d) p.176 ###
f_error2<- LOGSAL - fore3
hist(f_error2)
summary(f_error2)
 Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.19136  0.06171  0.18210  0.20063  0.33977  0.76745 
sd(f_error2)
[1] 0.1996943
sqrt(0.1996943^2+ 0.200624^2)
### The Job 3 can not forecasted by Job 1 and Job 2 ### 

