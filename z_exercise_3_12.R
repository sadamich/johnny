### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 3 12 (p.183)                                                  ###
set.seed(1)
### Normally distributed random veriables ###
epsilon <- rnorm(100,0,1)
omega <- rnorm(100,0,1)
eta<- rnorm(100,0,1)
### Transformations of 3 random variables ###
x_1 <- 5+ omega+ 0.3*eta
x_2 <- 10 + omega
x_3 <- 5 + eta
y <- x_1+x_2
z<- x_2+ x_3
### Korrelationen zwischen random variablen ###
cor(x_1, x_3)
[1] 0.2649096
cor(x_2, x_3)
[1] -0.04953621

### Regression ###
eq1<- lm(y ~ x_1+x_2)
summary(eq1)
### Call: ###
lm(formula = y ~ x_1 + x_2)
Residuals:
       Min         1Q     Median         3Q        Max 
-7.893e-15 -7.150e-16 -1.082e-16  4.495e-16  1.841e-14 
Coefficients:
             Estimate Std. Error   t value Pr(>|t|)    
(Intercept) 7.105e-15  4.098e-15 1.734e+00   0.0861 .  
x_1         1.000e+00  7.046e-16 1.419e+15   <2e-16 ***
x_2         1.000e+00  7.298e-16 1.370e+15   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 2.173e-15 on 97 degrees of freedom
Multiple R-squared:      1,     Adjusted R-squared:      1 
F-statistic: 3.888e+31 on 2 and 97 DF,  p-value: < 2.2e-16

### Regression ###
eq2<- lm(z ~ x_2+x_3)
summary(eq2)
Call:
lm(formula = z ~ x_2 + x_3)
Residuals:
       Min         1Q     Median         3Q        Max 
-9.682e-14  2.310e-16  8.720e-16  1.564e-15  7.215e-15 
Coefficients:
              Estimate Std. Error    t value Pr(>|t|)    
(Intercept) -1.563e-14  1.176e-14 -1.330e+00    0.187    
x_2          1.000e+00  1.045e-15  9.565e+14   <2e-16 ***
x_3          1.000e+00  9.682e-16  1.033e+15   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 9.952e-15 on 97 degrees of freedom
Multiple R-squared:      1,     Adjusted R-squared:      1 
F-statistic: 9.442e+29 on 2 and 97 DF,  p-value: < 2.2e-16

### Regression ###
eq3<- lm(y ~ x_1)
summary(eq3)
### Call:###
lm(formula = y ~ x_1)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.72400 -0.19054  0.00398  0.21433  0.75738 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.40294    0.15439   35.00   <2e-16 ***
x_1          1.91715    0.03046   62.94   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3007 on 98 degrees of freedom
Multiple R-squared:  0.9759,    Adjusted R-squared:  0.9756 
F-statistic:  3961 on 1 and 98 DF,  p-value: < 2.2e-16

### Regression  ###
eq4<- lm(z ~ x_3)
summary(eq4)
### Regression Call: ###
lm(formula = z ~ x_3)
Residuals:
    Min      1Q  Median      3Q     Max 
-1.8455 -0.6464 -0.1507  0.5328  2.3325 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 10.19294    0.47971   21.25   <2e-16 ***
x_3          0.95412    0.09344   10.21   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9616 on 98 degrees of freedom
Multiple R-squared:  0.5155,    Adjusted R-squared:  0.5105 
F-statistic: 104.3 on 1 and 98 DF,  p-value: < 2.2e-16