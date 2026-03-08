### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm702<- read.csv("xm702.csv", header = TRUE)
str(xm702)
attach(xm702)
logdj_1<- c(NA,LOGDJ)[1:2528]
dlogdj_1<- c(NA,DLOGDJ)[1:2528]
dlogdj_2<- c(NA, dlogdj_1)[1:2528]
dlogdj_3<- c(NA, dlogdj_2)[1:2528]
dlogdj_4<- c(NA, dlogdj_3)[1:2528]
dlogdj_5<- c(NA, dlogdj_4)[1:2528]
t<- 1:2528
eq_adf<- lm(DLOGDJ ~ logdj_1 + dlogdj_1 + dlogdj_2 + dlogdj_3
            + dlogdj_4 + dlogdj_5 + t)
summary(eq_adf)
### Panel 1 (p.603)Call:lm(formula = DLOGDJ ~ logdj_1 + dlogdj_1 +         ###
###                  dlogdj_2 + dlogdj_3 + dlogdj_4 + dlogdj_5 + t)        ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.074934 -0.004619 -0.000072  0.004889  0.046346 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
(Intercept)  3.422e-02  1.342e-02   2.551  0.01081 * 
logdj_1     -4.428e-03  1.744e-03  -2.539  0.01117 * 
dlogdj_1     3.098e-02  1.992e-02   1.555  0.12010   
dlogdj_2    -1.698e-02  1.993e-02  -0.852  0.39433   
dlogdj_3    -4.469e-02  1.991e-02  -2.245  0.02485 * 
dlogdj_4    -6.859e-03  1.992e-02  -0.344  0.73067   
dlogdj_5    -1.351e-02  1.992e-02  -0.678  0.49755   
t            3.043e-06  1.092e-06   2.787  0.00535 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.008901 on 2514 degrees of freedom
  (6 observations deleted due to missingness)
Multiple R-squared:  0.00697,   Adjusted R-squared:  0.004205 
F-statistic: 2.521 on 7 and 2514 DF,  p-value: 0.0139
anova(eq_adf)
Analysis of Variance Table
Response: DLOGDJ
            Df   Sum Sq    Mean Sq F value   Pr(>F)   
logdj_1      1 0.000039 0.00003852  0.4863 0.485660   
dlogdj_1     1 0.000184 0.00018391  2.3214 0.127728   
dlogdj_2     1 0.000074 0.00007379  0.9314 0.334582   
dlogdj_3     1 0.000427 0.00042739  5.3948 0.020277 * 
dlogdj_4     1 0.000015 0.00001476  0.1864 0.666008   
dlogdj_5     1 0.000044 0.00004400  0.5554 0.456171   
t            1 0.000616 0.00061555  7.7698 0.005353 **
Residuals 2514 0.199166 0.00007922                    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
eq_adf2<- lm(DLOGDJ ~ dlogdj_1 + dlogdj_2 + dlogdj_3
            + dlogdj_4 + dlogdj_5)
summary(eq_adf2)
anova(eq_adf,eq_adf2)
### Panel 2 (p.603) Analysis of Variance Table                             ###
Model 1: DLOGDJ ~ logdj_1 + dlogdj_1 + dlogdj_2 + dlogdj_3 + dlogdj_4 + 
    dlogdj_5 + t
Model 2: DLOGDJ ~ dlogdj_1 + dlogdj_2 + dlogdj_3 + dlogdj_4 + dlogdj_5
  Res.Df     RSS Df   Sum of Sq      F  Pr(>F)  
1   2514 0.19917                                
2   2516 0.19983 -2 -0.00066567 4.2012 0.01508 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
res<- resid(eq_adf)
### Panel 3 (p.603)                                                        ###
acf(res)

### I(2) ???                                                               ###
str(xm702)
DLOGDJ_1<- c(NA,DLOGDJ)[1:2528]
Ddlogdj<- DLOGDJ - DLOGDJ_1
str(Ddlogdj)
t<- 1:2528
eq_adf_u2<- lm(Ddlogdj ~ dlogdj_1+ t)
summary(eq_adf_u2)
### Panel 4 (p.603) Call:lm(formula = Ddlogdj ~ dlogdj_1 + t)               ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.074821 -0.004537  0.000050  0.004922  0.047922 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.281e-04  3.553e-04   0.360    0.719    
dlogdj_1    -9.705e-01  1.990e-02 -48.766   <2e-16 ***
t            3.266e-07  2.433e-07   1.342    0.180    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.008915 on 2523 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared:  0.4852,    Adjusted R-squared:  0.4848 
F-statistic:  1189 on 2 and 2523 DF,  p-value: < 2.2e-16
anova(eq_adf_u2)
eq_adf_u22<- lm(Ddlogdj ~ 1)
summary(eq_adf_u22)
anova(eq_adf_u2, eq_adf_u22)
anova(eq_adf_u2, eq_adf_u22)
### Panel 5 (p.603) Analysis of Variance Table                             ###
Model 1: Ddlogdj ~ dlogdj_1 + t
Model 2: Ddlogdj ~ 1
  Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
1   2523 0.20051                                  
2   2525 0.38950 -2  -0.18899 1189.1 < 2.2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.
res<- resid(eq_adf_u2)
### Panel 6 (p.603)                                                        ###
acf(res)
