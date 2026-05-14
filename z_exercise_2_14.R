### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 2 14 (p.115)
xr111<- read.csv("xr111.csv",header=TRUE)
str(xr111)
attach(xr111)
### Problem (a) and (b)
eq<- lm(FGPA ~ SATM)
summary(eq)
Call:lm(formula = FGPA ~ SATM)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.66124 -0.30806 -0.03127  0.25070  1.01576 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.3287     2.9405  -0.112    0.914
SATM          0.5176     0.5035   1.028    0.334
Residual standard error: 0.5379 on 8 degrees of freedom
Multiple R-squared:  0.1167,    Adjusted R-squared:  0.006248 
F-statistic: 1.057 on 1 and 8 DF,  p-value: 0.3341

### Problem (c)
confint(eq)
                 2.5 %   97.5 %
(Intercept) -7.1095119 6.452117
SATM        -0.6435615 1.678724

### Problem (d) forecast for students (SATM = 6)                          ###
f<- -0.3287 +0.5176*SATM[SATM==6]
f
[1] 2.7769 2.7769
FGPA[SATM==6]
[1] 2.566 3.225

