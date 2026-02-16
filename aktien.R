aktien<- read.csv("aktien.csv", header=TRUE)
str(aktien)
attach(aktien)
str(cyber)
x<- cyber
plot(x, typ="l")
t<- 1:25
kurs<- lm(x ~ t)
summary(kurs)
### Estimation of trend model                                              ###
Call: lm(formula = x ~ t)
Residuals:
    Min      1Q  Median      3Q     Max 
-19.615  -8.766  -3.125   6.668  22.725 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 218.8600     5.0358   43.46  < 2e-16 ***
t             3.6415     0.3387   10.75 1.92e-10 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 12.21 on 23 degrees of freedom
Multiple R-squared:  0.834,     Adjusted R-squared:  0.8268 
F-statistic: 115.6 on 1 and 23 DF,  p-value: 1.92e-10
