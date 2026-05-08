### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr318<- read.csv("xr318.csv", header =TRUE)
str(xr318)
attach(xr318)
y<- log(SGAS/PGAS)
x2<- log(INC/PALL)
x3<- log(PGAS/PALL)
x4<- log(PPUB/PALL)
x5<- log(PNCAR/PALL)
x6<- log(PUCAR/PALL)

eq<- lm(y~ x2+x3+x4+x5+x6)
summary(eq)
eq_r<- lm(y~ x2+x3+x4)
summary(eq_r)
anova(eq, eq_r)
Analysis of Variance Table
Model 1: y ~ x2 + x3 + x4 + x5 + x6
Model 2: y ~ x2 + x3 + x4
  Res.Df      RSS Df  Sum of Sq      F Pr(>F)
1     24 0.012512                            
2     26 0.014790 -2 -0.0022789 2.1857 0.1343 (not joint significant)

eq_b<- lm(y~ log(PGAS)+log(PALL)+log(INC)+log(PPUB))
summary(eq_b)
### The price elasticity of gasolin consumption 
### Call:lm(formula = y ~ log(PGAS) + log(PALL) + log(INC) + log(PPUB))    ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.036640 -0.010979  0.002528  0.010866  0.024017 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.03921    0.19223  21.012  < 2e-16 ***
log(PGAS)   -0.29668    0.05215  -5.689 6.34e-06 ***
log(PALL)   -1.55275    0.31230  -4.972 4.01e-05 ***
log(INC)     1.32876    0.14707   9.035 2.39e-09 ***
log(PPUB)    0.17679    0.06693   2.641    0.014 * 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.01715 on 25 degrees of freedom
Multiple R-squared:  0.9938,    Adjusted R-squared:  0.9929 
F-statistic:  1009 on 4 and 25 DF,  p-value: < 2.2e-16
res<- resid(eq_b)
ssr<- sum(res^2)
ssr
0.007355256

SUM<- -log(PALL)+log(PPUB)+log(PGAS)+log(INC)
eq_c<- lm(y~ log(PALL)+SUM)
summary(eq_c)
res_c<- resid(eq_c)
ssr_c<- sum(res_c^2)
ssr_c

F<- function(ssr_r,ssr,g,n,k){
result<- ((ssr_r-ssr)/g)/(ssr/(n-k))
return(result)
}
F( 0.1447376 ,0.007355256,1,30,5)


library(car)
linearHypothesis(eq_b, "1*log(PGAS) + 1*log(PALL)+1*log(INC)+1*log(PPUB) = 0")
r hypothesis test:
log(PGAS)  + log(PALL)  + log(INC)  + log(PPUB) = 0
Model 1: restricted model
Model 2: y ~ log(PGAS) + log(PALL) + log(INC) + log(PPUB)

  Res.Df       RSS Df Sum of Sq      F    Pr(>F)    
1     26 0.0147905                                  
2     25 0.0073553  1 0.0074352 25.272 3.475e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


### Problem (d) (p.186)                                                   ###                     
linearHypothesis(eq_b, "1*log(PALL)+1*log(INC)+1*log(PPUB) = 0")
Linear hypothesis test:
log(PALL)  + log(INC)  + log(PPUB) = 0
Model 1: restricted model
Model 2: y ~ log(PGAS) + log(PALL) + log(INC) + log(PPUB)
  Res.Df       RSS Df  Sum of Sq      F Pr(>F)
1     26 0.0074022                            
2     25 0.0073553  1 4.6927e-05 0.1595  0.693

eq_d<- lm(y~ log(PGAS)+x2+x4)
summary(eq_d)
Call:lm(formula = y ~ log(PGAS) + x2 + x4)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.035442 -0.010769  0.003703  0.010092  0.024626 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.10575    0.09433  43.526   <2e-16 ***
log(PGAS)   -0.31697    0.01160 -27.324   <2e-16 ***
x2           1.27185    0.03580  35.523   <2e-16 ***
x4           0.15672    0.04348   3.604   0.0013 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.01687 on 26 degrees of freedom
Multiple R-squared:  0.9938,    Adjusted R-squared:  0.9931 
F-statistic:  1391 on 3 and 26 DF,  p-value: < 2.2e-16
res_d<- resid(eq_d)
ssr_d<- sum(res_d^2)
ssr_d

F<- function(ssr_r,ssr,g,n,k){
result<- ((ssr_r-ssr)/g)/(ssr/(n-k))
return(result)
}
F(0.007402183,0.007355256,1,30,5)
[1] 0.1595016 ### The same coefficient with the parameter restriction test ###
confint(eq_d)
                 2.5 %     97.5 %
(Intercept)  3.9118503  4.2996445
log(PGAS)   -0.3408097 -0.2931210
x2           1.1982530  1.3454417
x4           0.0673357  0.2461045
