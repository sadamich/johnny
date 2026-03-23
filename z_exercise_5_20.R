### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
set.seed(200)
x<- rnorm(100, 0,1)
eta<- rnorm(100, 0, x^2)
y<- x+eta
plot(y)
hist(y)
acf(y)
### problem (a) OLS
eq<- lm(y ~ x)
summary(eq)
Call:lm(formula = y ~ x)
Residuals:
    Min      1Q  Median      3Q     Max 
-4.9908 -0.1902 -0.0226  0.1629  8.7854 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.01035    0.13339   0.078    0.938    
x            1.49908    0.14568  10.290   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.333 on 98 degrees of freedom
Multiple R-squared:  0.5194,    Adjusted R-squared:  0.5144 
F-statistic: 105.9 on 1 and 98 DF,  p-value: < 2.2e-16
res<- resid(eq)
plot(res)

eq_wls<- lm(y ~ x, weigth= x^2)
summary(eq_wls)