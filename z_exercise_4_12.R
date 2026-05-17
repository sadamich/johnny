### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 12
x<- runif(50, 0,2)
eta<- rt(50,3)
y<- 1/2+x+eta
eq<- lm(y ~ x)
summary(eq) 
Call:
lm(formula = y ~ x)
Residuals:
    Min      1Q  Median      3Q     Max 
-5.8207 -1.0058 -0.1308  0.7127  5.4992 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.05329    0.47804   0.111    0.912   
x            1.28580    0.42517   3.024    0.004 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.772 on 48 degrees of freedom
Multiple R-squared:   0.16,     Adjusted R-squared:  0.1425 
F-statistic: 9.146 on 1 and 48 DF,  p-value: 0.003995
res<- resid(eq)
plot(res)
hist(res)
