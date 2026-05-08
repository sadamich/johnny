### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 3 14 (p.183)                                                  ###
xr314<- read.csv("xr314.csv", header = TRUE)
str(xr314)
attach(xr314)

### Problem (a) Korrelationen                                              ###
cor(FGPA, SATM)
[1] 0.1950404
cor(FGPA, SATV)
[1] 0.09216712
cor(FGPA, FEM)
[1] 0.1764907
cor(SATM, SATV)
[1] 0.2878011
cor(SATM, FEM)
[1] -0.1626804
cor(SATV, FEM)
[1] 0.03357664

### Problem (b) Regression                                                 ###
eqFGPA<- lm(FGPA ~ SATV)
summary(eqFGPA)
Call: lm(formula = FGPA ~ SATV)
Residuals:
     Min       1Q   Median       3Q      Max 
-1.38333 -0.30694 -0.02763  0.32359  1.14037 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.44173    0.15506   15.75   <2e-16 ***
SATV         0.06309    0.02766    2.28   0.0229 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.4587 on 607 degrees of freedom
Multiple R-squared:  0.008495,  Adjusted R-squared:  0.006861 
F-statistic: 5.201 on 1 and 607 DF,  p-value: 0.02293

resFGPA<- resid(eqFGPA)
sum(resFGPA^2)
[1] 127.6917
str(FEM)

eqFGPA2<- lm(FGPA ~ SATM+SATV+FEM)
summary(eqFGPA2)
### Regression  Call:###
lm(formula = FGPA ~ SATM + SATV + FEM)
Residuals:
     Min       1Q   Median       3Q      Max 
-1.31351 -0.29883 -0.02146  0.29419  1.09966 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.55705    0.21610   7.205 1.73e-12 ***
SATM         0.17274    0.03193   5.410 9.07e-08 ***
SATV         0.01416    0.02793   0.507    0.612    
FEM          0.20027    0.03738   5.358 1.20e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.4418 on 605 degrees of freedom
Multiple R-squared:  0.08296,   Adjusted R-squared:  0.07842 
F-statistic: 18.24 on 3 and 605 DF,  p-value: 2.411e-11

resFGPA2<- resid(eqFGPA2)
sum(resFGPA2^2)
[1] 118.101

### Problem (c) SATV is not significant in the model (eqFGPA2)            ###                                                        

### Explanatory variables SATM,SATV,FEM:cf(p.160)                         ###

eq_expl<- lm(SATM~SATV+FEM)
summary(eq_expl)
Call:lm(formula = SATM ~ SATV + FEM)
Residuals:
    Min      1Q  Median      3Q     Max 
-1.8693 -0.3504  0.0266  0.3607  1.6688 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.88253    0.19042  25.641  < 2e-16 ***
SATV         0.26013    0.03393   7.668 7.00e-14 ***
FEM         -0.21082    0.04678  -4.506 7.92e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.5622 on 606 degrees of freedom
Multiple R-squared:  0.1126,    Adjusted R-squared:  0.1096 
F-statistic: 38.43 on 2 and 606 DF,  p-value: < 2.2e-16

### Inflation factor ###
inflation factor
1/ sqrt(1-0.1126)
[1] 1.06155

eq_expl2<- lm(SATV~SATM+FEM)
summary(eq_expl2)
Call:lm(formula = SATV ~ SATM + FEM)
Residuals:
     Min       1Q   Median       3Q      Max 
-1.96228 -0.44815 -0.04225  0.41386  1.99774 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.39636    0.28243  12.025   <2e-16 ***
SATM         0.33998    0.04434   7.668    7e-14 ***
FEM          0.11388    0.05418   2.102    0.036 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.6427 on 606 degrees of freedom
Multiple R-squared:  0.08947,   Adjusted R-squared:  0.08646 
F-statistic: 29.77 on 2 and 606 DF,  p-value: 4.638e-13
### Inflation factor ###
inflation factor
1/ sqrt(1-0.08947)
[1] 1.04798

eq_expl3<- lm(FEM~SATV+SATM)
summary(eq_expl3)
Call:lm(formula = FEM ~ SATV + SATM)
Residuals:
    Min      1Q  Median      3Q     Max 
-0.6751 -0.3908 -0.2922  0.5657  0.8074 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.99473    0.23133   4.300 1.99e-05 ***
SATV         0.06356    0.03024   2.102    0.036 *  
SATM        -0.15379    0.03413  -4.506 7.92e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.4801 on 606 degrees of freedom
Multiple R-squared:  0.03351,   Adjusted R-squared:  0.03032 
F-statistic: 10.51 on 2 and 606 DF,  p-value: 3.27e-05
### Inflation factor ###
inflation factor
1/ sqrt(1-0.03351)
[1] 1.017188


