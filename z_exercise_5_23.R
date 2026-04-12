### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr523<- read.csv("xr523.csv", header=TRUE)
str(xr523)
attach(xr523)
detach(xr523)
### Problem (a) OLS estimate                                               ###
eq<- lm(CRIME ~ POLICE)
summary(eq)
Call:lm(formula = CRIME ~ POLICE)
Residuals:
    Min      1Q  Median      3Q     Max 
-4.4668 -1.0354  0.1969  1.2026  2.9202 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -7.5579     1.0955  -6.899 5.19e-10 ***
POLICE        1.6822     0.1071  15.701  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.619 on 98 degrees of freedom
Multiple R-squared:  0.7155,    Adjusted R-squared:  0.7126 
F-statistic: 246.5 on 1 and 98 DF,  p-value: < 2.2e-16
res_ols<- resid(eq)
### Problem (b) Dummy variable                                             ###
eq_d<- lm(POLICE ~ ELECTION)
summary(eq_d)
Call:lm(formula = POLICE ~ ELECTION)
Residuals:
    Min      1Q  Median      3Q     Max 
-3.5723 -0.9512  0.0313  0.9547  3.4140 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   9.8475     0.1679  58.645  < 2e-16 ***
ELECTION      1.0599     0.3358   3.156  0.00212 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.454 on 98 degrees of freedom
Multiple R-squared:  0.09226,   Adjusted R-squared:  0.083 
F-statistic: 9.961 on 1 and 98 DF,  p-value: 0.002125
fit <- fitted(eq_d)
resaux<- resid(eq_d)

y1<- CRIME[ELECTION==1]
summary(y1)
y1_m<-  8.764 
y0<- CRIME[ELECTION==0]
summary(y0)
y0_m<- mean(y0)
x1<- POLICE[ELECTION==1]
x1_m<- mean(x1)
x0<- POLICE[ELECTION==0]
x0_m<- mean(x0)
beta_iv<- (y1_m - y0_m)/(x1_m - x0_m)
beta_iv
[1] -0.8669508
###Problem (d)                                                             ###
eq_iv<- lm(CRIME ~ fit)
summary(eq_iv)
### IV estimate Call:lm(formula = CRIME ~ fit)                             ###
Residuals:
    Min      1Q  Median      3Q     Max 
-6.7427 -2.2366  0.0853  2.2192  7.5823 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  18.2210     6.6358   2.746  0.00718 **
fit          -0.8670     0.6555  -1.323  0.18903   
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 3.009 on 98 degrees of freedom
Multiple R-squared:  0.01754,   Adjusted R-squared:  0.007513 
F-statistic: 1.749 on 1 and 98 DF,  p-value: 0.189
### beta_iv -0.8669508 = fit -0.8670                                       ###

### Hausman test (p.411)                                                   ###
res_ols<- resid(eq)
resaux<- resid(eq_d)
eq_hausman<- lm(res_ols ~ POLICE+resaux)
summary(eq_hausman)
Call:lm(formula = res_ols ~ POLICE + resaux)
Residuals:
     Min       1Q   Median       3Q      Max 
-2.64585 -0.62712  0.00969  0.84928  2.22349 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  25.7790     2.3061   11.18   <2e-16 ***
POLICE       -2.5492     0.2278  -11.19   <2e-16 ***
resaux        2.8083     0.2391   11.74   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.046 on 97 degrees of freedom
Multiple R-squared:  0.5871,    Adjusted R-squared:  0.5786 
F-statistic: 68.98 on 2 and 97 DF,  p-value: < 2.2e-16
LM = n*R^2= 100*0.5871 = 58.71 HO (Exogenity) is rejected.                 ###
dchisq(x, df, ncp = 0, log = FALSE)
dchisq(0.025,1)
[1] 2.49179
dchisq(0.05,1)
[1] 1.740074