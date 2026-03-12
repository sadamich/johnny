### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 26 (p.721)                                                  ###
xr726 <- read.csv("xr726.csv", header =TRUE)
str(xr726)
attach(xr726)
eq_p<- lm(LOGPT ~ LOGQT+LOGRIT)
summary(eq_p)
### Price equation Call:lm(formula = LOGPT ~ LOGQT + LOGRIT)               ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.41043 -0.19581 -0.02261  0.17310  0.52072 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -6.1886     1.6559  -3.737 0.000504 ***
LOGQT        -0.7945     0.1114  -7.135 5.11e-09 ***
LOGRIT        0.9231     0.2285   4.040 0.000196 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2605 on 47 degrees of freedom
Multiple R-squared:  0.544,     Adjusted R-squared:  0.5245 
F-statistic: 28.03 on 2 and 47 DF,  p-value: 9.7e-09

### parameter restriction                                        ###


