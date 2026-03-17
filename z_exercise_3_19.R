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
SUM<- (log(PGAS)+log(PALL)+log(INC)+log(PPUB))
eq_c<- lm(y~ SUM)
summary(eq_c)
### The parameter restriction Call:lm(formula = y ~ SUM)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.31271 -0.02331  0.02229  0.08930  0.20022 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.43739    0.25992  20.920  < 2e-16 ***
SUM          0.06937    0.01209   5.737 3.72e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1401 on 28 degrees of freedom
Multiple R-squared:  0.5403,    Adjusted R-squared:  0.5239 
F-statistic: 32.91 on 1 and 28 DF,  p-value: 3.725e-06
### H0 (SUM = 0) is rejectet                                              ###
SUM2<- (log(PALL)+log(INC)+log(PPUB))
eq_d<- lm(y~ SUM2)
summary(eq_d)???
