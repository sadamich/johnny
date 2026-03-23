### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
set.seed(207)
x<- rnorm(100, 0, 1)
x1<- c(NA, x)
w<- rnorm(100,0,1)
x<- x1[1:100] + w
e<- rnorm(100, 0, 1)
e1<- c(NA,e)
eta<- rnorm(100,0,1)
e<- e1[1:100] + eta
y<- x + e
eq<- lm(y ~ x)
summary(eq)

Call:lm(formula = y ~ x)
Residuals:
    Min      1Q  Median      3Q     Max 
-3.1644 -1.2303 -0.0492  1.0767  3.5084 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.20518    0.14186  -1.446    0.151    
x            1.06097    0.09766  10.864   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.411 on 97 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.5489,    Adjusted R-squared:  0.5442 
F-statistic:   118 on 1 and 97 DF,  p-value: < 2.2e-16