### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr315<- read.csv("xr315.csv", header =TRUE)
str(xr315)
attach(xr315)
eq_base<- lm(LOGY ~ LOGL + LOGK)
summary(eq_base)
### Proble a Base Model Regression Call:lm(formula = LOGY ~ LOGL + LOGK)   ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.65794 -0.16933 -0.04051  0.16689  0.62780 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.70083    0.41524   1.688   0.1050    
LOGL         0.75632    0.09145   8.270 2.42e-08 ***   significant
LOGK         0.24182    0.11017   2.195   0.0385 *     significant
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2817 on 23 degrees of freedom
Multiple R-squared:  0.9569,    Adjusted R-squared:  0.9531 
F-statistic: 255.2 on 2 and 23 DF,p-value:2.2e-16 (H0 b2 = b3 = 0 is rejectet)
res<- resid(eq_base)
ssr<- sum(res^2)
ssr
[1] 1.825544
library(car)
linearHypothesis(eq_base, "1*LOGL + 1*LOGK = 0")
Linear hypothesis test:
LOGL  + LOGK = 0
Model 1: restricted model
Model 2: LOGY ~ LOGL + LOGK
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1     24 32.789                                  
2     23  1.826  1    30.964 390.11 6.352e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### The same (equal) coefficients Model                                    ### 
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
ssr<- sum(res^2)
ssr

### the constant returns to scale Model                                    ###
log_lk_c<- LOGL - LOGK
eq_c<- lm(LOGY - LOGK ~ log_lk_c)
summary(eq_c)
res_c<- resid(eq_c)
ssr_c<- sum(res_c^2)
ssr_c
[1] 1.825652
### The constant return to scal model                                      ###
### lm(formula = LOGY - LOGK ~ log_lk_c)                                   ###
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

### F tests                                                                ###
anova(eq_base, eq_c)

F<- function(n,k,g,R,R_r){
result<- ((n-k)/g)*(R-R_r)/(1-R)
return(result)
}
F(26,3,1, 0.95688,0.751397)


F<- function(n,k, g, ssr_r,ssr){
result<- ((ssr_r - ssr)/g)/(ssr/(n-k))
return(result)
}
F(26,3,1,1.825652,1.825544)


[1] 0.0006803451

F<- function(n, k, R){
result<- (n-k)/(k-1)* R/(1-R)
return(result)
}
F(26, 2, 0.7514)
[1] 72.54063
F-statistic: 72.54 on 1 and 24 DF,  p-value: 1.023e-08
