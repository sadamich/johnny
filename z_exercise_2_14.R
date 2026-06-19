### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 2 14 (p.115)
xr111<- read.csv("xr111.csv",header=TRUE)
str(xr111)
attach(xr111)
plot(FGPA,SATM)
### Problem (a) and (b)
eq<- lm(FGPA ~ SATM)
hist(FGPA)
hist(SATM)
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
res<- resid(eq)
ssr<- sum(res^2)
### Problem (c)
confint(eq)
                 2.5 %   97.5 %
(Intercept) -7.1095119 6.452117
SATM        -0.6435615 1.678724

sqrt(ssr/8/sum((SATM-mean(SATM))^2))
[1] 0.5035301      sd(SATM)

0.5176- 1.94*0.5035
[1] -0.45919
0.5176 + 1.94*0.5035
[1] 1.49439

### Problem (d) forecast for students (SATM = 6)                          ###
### full sample                                                           ###
f<- -0.3287 +0.5176*SATM[SATM==6]
f
[1] 2.7769                 2.7769
FGPA[SATM==6]
[1] 2.566                  3.225
    (overestimated)        (underestimated)

u<- c(4,8)
SATM[4]
[1] 6
SATM[8]
[1] 6

### The regression without the elements of SATM=6                         ###
eq_8<- lm(FGPA[-u] ~ SATM[-u])
summary(eq_8)
Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.08517    3.28648  -0.026    0.980
SATM[-u]     0.47037    0.56673   0.830    0.438
res_8<- resid(eq_8)
ssr_8<- sum(res_8^2)
ssr_8/8*(1+ 1/8+ (6- mean(SATM[-u])^2/sum(SATM[-u]- mean(SATM[-u])^2)))
[1] 1.87322

f_8<- -0.08517+0.47037*SATM[SATM==6]
f_8
[1] 2.73705        2.73705
FGPA[SATM==6]
[1] 2.566          3.225


2.73705 - 1.94*sqrt( 1.87322)
[1] 0.08185683
2.73705 + 1.94*sqrt( 1.87322)
5.392243