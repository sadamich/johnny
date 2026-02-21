### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 19 (p.718)                                                  ###
xm722 <- read.csv("xm722.csv", header =TRUE)
str(xm722)
attach(xm722)
r<- lm(DUS3MT[2:624] ~ US3MTBIL[1:623])
summary(r)
Call: lm(formula = DUS3MT[2:624] ~ US3MTBIL[1:623])
Residuals:
    Min      1Q  Median      3Q     Max 
-4.7302 -0.1346 -0.0178  0.1460  2.8131 
Coefficients:
                 Estimate Std. Error t value Pr(>|t|)  
(Intercept)      0.078463   0.037163   2.111   0.0351 *
US3MTBIL[1:623] -0.014162   0.006349  -2.231   0.0261 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.4669 on 621 degrees of freedom
Multiple R-squared:  0.007948,  Adjusted R-squared:  0.00635 
F-statistic: 4.975 on 1 and 621 DF,  p-value: 0.02607
### a Residuals                                                            ###
res<- resid(r)
plot(res)
hist(res)
acf(res)
acf(res^2)
### ARCH by ols                                                            ###
str(res)
res_sq<- res^2
res_sq<- res_sq[2:623]
res_lag<- res[1:622]
res_lag_sq<- res_lag^2
e_ols<- lm(res_sq ~ res_lag_sq)
summary(e_ols)
Call: lm(formula = res_sq ~ res_lag_sq)
Residuals:
    Min      1Q  Median      3Q     Max 
-1.9489 -0.1579 -0.1494 -0.1089 21.6834 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.15737    0.04509   3.490 0.000517 ***
res_lag_sq   0.27695    0.03859   7.177 2.05e-12 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.105 on 620 degrees of freedom
Multiple R-squared:  0.0767,    Adjusted R-squared:  0.07522 
F-statistic: 51.51 on 1 and 620 DF,  p-value: 2.045e-12





