### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 10 (p.269)                                                  ###
x<- rnorm(100, 0, 1)
x_sq<- x^2
w<- rnorm(100, 0, 1)
y<- 1+x_sq+w

eq<- lm(y ~ x+x_sq)
summary(eq)
### the unrestricted Model :lm(formula = y ~ x + x_sq)                      ###
Residuals:
    Min      1Q  Median      3Q     Max 
-2.3091 -0.7772  0.0184  0.6431  3.1875 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.27944    0.12029  10.636   <2e-16 ***
x           -0.03823    0.11005  -0.347    0.729    
x_sq         1.06256    0.08200  12.958   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9871 on 97 degrees of freedom
Multiple R-squared:  0.6477,    Adjusted R-squared:  0.6404 
F-statistic: 89.17 on 2 and 97 DF,  p-value: < 2.2e-16

eq_r<- lm(y~ x_sq)
summary(eq_r)
### The restricted Model :lm(formula = y ~ x_sq)
Residuals:
    Min      1Q  Median      3Q     Max 
-2.3328 -0.7215  0.0178  0.6066  3.2006 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.27511    0.11911   10.71   <2e-16 ***
x_sq         1.06873    0.07969   13.41   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9826 on 98 degrees of freedom
Multiple R-squared:  0.6473,    Adjusted R-squared:  0.6437 
F-statistic: 179.8 on 1 and 98 DF,  p-value: < 2.2e-16
res_r<- resid(eq_r)

eq_aux<- lm(res_r ~ x+x_sq)
summary(eq_aux)
Call:lm(formula = res_r ~ x + x_sq)
Residuals:
    Min      1Q  Median      3Q     Max 
-2.3091 -0.7772  0.0184  0.6431  3.1875 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)
(Intercept)  0.004331   0.120291   0.036    0.971
x           -0.038231   0.110050  -0.347    0.729
x_sq        -0.006169   0.082000  -0.075    0.940
Residual standard error: 0.9871 on 97 degrees of freedom
Multiple R-squared:  0.001243,  Adjusted R-squared:  -0.01935 
F-statistic: 0.06034 on 2 and 97 DF,  p-value: 0.9415

LM = 100*0.001243=  0.1243
### H0 (b2=0) is not rejected.