### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr315<- read.csv("xr315.csv", header =TRUE)
str(xr315)
attach(xr315)
eq_base<- lm(LOGY ~ LOGL + LOGK)
summary(eq_base)
### Base Model Regression Call:lm(formula = LOGY ~ LOGL + LOGK)            ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.65794 -0.16933 -0.04051  0.16689  0.62780 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.70083    0.41524   1.688   0.1050    
LOGL         0.75632    0.09145   8.270 2.42e-08 ***
LOGK         0.24182    0.11017   2.195   0.0385 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2817 on 23 degrees of freedom
Multiple R-squared:  0.9569,    Adjusted R-squared:  0.9531 
F-statistic: 255.2 on 2 and 23 DF,p-value:2.2e-16 (H0 b2 = b3 = 0 is rejectet)
res<- resid(eq_base)
e<- sum(res^2)
### The same 
log_lk<- LOGL+LOGK
eq_same<- lm(LOGY ~ log_lk)
summary(eq_same)
#### Call:lm(formula = LOGY ~ log_lk)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.71889 -0.25799  0.07021  0.23644  0.39834 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.01014    0.35835   0.028    0.978    
log_lk       0.52432    0.02607  20.111   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3144 on 24 degrees of freedom
Multiple R-squared:  0.944,     Adjusted R-squared:  0.9416 
F-statistic: 404.4 on 1 and 24 DF,  p-value: < 2.2e-16
res<- resid(eq_same)
e_r<- sum(res^2)
e_r
log_lk_c<- LOGL - LOGK
eq_c<- lm(LOGY - LOGK ~ log_lk_c)
summary(eq_c)

### The constant return to scal model :lm(formula = LOGY - LOGK ~ log_lk_c)###
Residuals:
    Min      1Q  Median      3Q     Max 
-0.6564 -0.1679 -0.0411  0.1665  0.6268 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.68636    0.13194   5.202 2.49e-05 ***
log_lk_c     0.75587    0.08875   8.517 1.02e-08 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2758 on 24 degrees of freedom
Multiple R-squared:  0.7514,    Adjusted R-squared:  0.741 
F-statistic: 72.54 on 1 and 24 DF,  p-value: 1.023e-08
res<- resid(eq_c)
e_r<- sum(res^2)
anova(eq_base, eq_same)
Analysis of Variance Table
Model 1: LOGY ~ LOGL + LOGK
Model 2: LOGY ~ log_lk
  Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
1     23 1.8255                              
2     24 2.3720 -1  -0.54645 6.8847 0.01518 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

F<- function(n, k, g, R1,R2){
result<- (n-k)/g * (R1 - R2)/(1-R1)
return(result)
}
F(26,3,1,0.9569,0.944)
[1] 6.883991

F<- function(e_r, e,n, k, g){
result<- ((e_r - e)/g) / (e /(n-k))
return(result)
}
F(2.371989,1.825544,26,3,1)
[1] 6.884652

### joint singnificance F Test                                            ###
F<- function(n, k, R){
result<- (n-k)/(k-1)* R/(1-R)
return(result)
}
F(26,2, 0.944)
F-statistic: 404.4 on 1 and 24 DF,  p-value: < 2.2e-16F<- function(n, k, R){


anova(eq_base, eq_c) 
Analysis of Variance Table
Response: LOGY
          Df Sum Sq Mean Sq  F value  Pr(>F)    
LOGL       1 40.137  40.137 505.6820 < 2e-16 ***
LOGK       1  0.382   0.382   4.8178 0.03853 *  
Residuals 23  1.826   0.079                     
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"LOGY - LOGK"’ removed because response
 differs from model 1

F<- function(n, k, g, R1,R2){
result<- (n-k)/g * (R1 - R2)/(1-R1)
return(result)
}
F(26,3,2,0.9569, 0.7514)
[1] 10.64293

F<- function(e_r, e,n, k, g){
result<- ((e_r - e)/g) / (e /(n-k))
return(result)
}
F(1.825652,1.825544,26,3,2)
[1] 0.0006803451


F<- function(n, k, R){
result<- (n-k)/(k-1)* R/(1-R)
return(result)
}
F(26, 2, 0.7514)
[1] 72.54063
F-statistic: 72.54 on 1 and 24 DF,  p-value: 1.023e-08
