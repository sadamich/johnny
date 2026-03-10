### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 25 (p.720)                                                  ###
xr725 <- read.csv("xr725.csv", header =TRUE)
str(xr725)
attach(xr725)
### Problem (a) OLS equation by equation                                   ###
eq1<- lm(SALES_1 ~ D2+D3+D4+ LOGA+LOGC)
summary(eq1)
Call:lm(formula = SALES_1 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
    Min      1Q  Median      3Q     Max 
-14.242  -5.936  -0.112   4.965  11.382 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -971.270    241.852  -4.016 0.000580 ***
D2            14.881      4.302   3.459 0.002232 ** 
D3            25.980      4.310   6.027 4.57e-06 ***
D4            60.818      4.407  13.799 2.60e-12 ***
LOGA         143.895     33.133   4.343 0.000261 ***
LOGC          70.494     20.254   3.480 0.002121 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 8.019 on 22 degrees of freedom
Multiple R-squared:  0.9061,    Adjusted R-squared:  0.8847 
F-statistic: 42.43 on 5 and 22 DF,  p-value: 1.417e-10
eq2<- lm(SALES_2 ~ D2+D3+D4 +LOGA+LOGC)
summary(eq2)
Call:lm(formula = SALES_2 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
    Min      1Q  Median      3Q     Max 
-14.591  -5.045   1.289   5.707  10.333 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   49.849    250.393   0.199  0.84403    
D2             1.295      4.454   0.291  0.77402    
D3            13.726      4.463   3.076  0.00553 ** 
D4            46.109      4.563  10.105 9.98e-10 ***
LOGA         -53.469     34.303  -1.559  0.13333    
LOGC          47.896     20.970   2.284  0.03238 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 8.302 on 22 degrees of freedom
Multiple R-squared:  0.869,     Adjusted R-squared:  0.8392 
F-statistic: 29.18 on 5 and 22 DF,  p-value: 5.196e-09
eq3<- lm(SALES_3 ~ D2+D3+D4+LOGA+LOGC)
summary(eq3)
Call:lm(formula = SALES_3 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
    Min      1Q  Median      3Q     Max 
-7.7343 -1.6939  0.2832  2.2130  4.4932 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -482.628    102.865  -4.692 0.000111 ***
D2             4.856      1.830   2.654 0.014500 *  
D3             4.125      1.833   2.250 0.034795 *  
D4            24.854      1.875  13.259 5.72e-12 ***
LOGA          84.353     14.092   5.986 5.04e-06 ***
LOGC          24.132      8.615   2.801 0.010405 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 3.411 on 22 degrees of freedom
Multiple R-squared:  0.9145,    Adjusted R-squared:  0.895 
F-statistic: 47.05 on 5 and 22 DF,  p-value: 5.105e-11
eq4<- lm(SALES_4 ~ D2+D3+D4+LOGA+LOGC)
summary(eq4)
Call:lm(formula = SALES_4 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
    Min      1Q  Median      3Q     Max 
-5.2584 -1.9061 -0.5506  1.2956  6.2801 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  84.8055   101.8651   0.833  0.41406    
D2           14.1249     1.8119   7.796 9.05e-08 ***
D3            5.5136     1.8155   3.037  0.00605 ** 
D4           25.9941     1.8563  14.003 1.94e-12 ***
LOGA          0.4743    13.9551   0.034  0.97319    
LOGC         -6.5191     8.5309  -0.764  0.45289    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 3.377 on 22 degrees of freedom
Multiple R-squared:  0.917,     Adjusted R-squared:  0.8981 
F-statistic: 48.59 on 5 and 22 DF,  p-value: 3.702e-11
eq5<- lm(SALES_5 ~ D2+D3+D4+LOGA+LOGC)
summary(eq5)
Call:lm(formula = SALES_5 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
    Min      1Q  Median      3Q     Max 
-15.050  -4.555  -1.699   4.878  15.509 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  544.632    271.644   2.005   0.0574 .  
D2             5.392      4.832   1.116   0.2764    
D3            10.641      4.841   2.198   0.0388 *  
D4            73.708      4.950  14.890  5.7e-13 ***
LOGA         -42.580     37.214  -1.144   0.2648    
LOGC         -45.469     22.750  -1.999   0.0581 .  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 9.007 on 22 degrees of freedom
Multiple R-squared:  0.9384,    Adjusted R-squared:  0.9244 
F-statistic: 67.07 on 5 and 22 DF,  p-value: 1.422e-12
### Standard errors are too large                                           ###
