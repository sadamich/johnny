### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 25 (p.720)                                                  ###
xr725 <- read.csv("xr725.csv", header =TRUE)
str(xr725)
attach(xr725)
### Problem (a) OLS equation by equation                                   ###
eq1<- lm(LOGSALES_1 ~ D2+D3+D4+ LOGA+LOGC)
summary(eq1)
Call:lm(formula = LOGSALES_1 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.211248 -0.063729 -0.003278  0.058533  0.147630 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -6.13969    2.87091  -2.139  0.04382 *  
D2           0.19320    0.05107   3.783  0.00102 ** 
D3           0.31359    0.05117   6.129 3.61e-06 ***
D4           0.61876    0.05232  11.827 5.25e-11 ***
LOGA         1.48867    0.39330   3.785  0.00102 ** 
LOGC         0.66019    0.24043   2.746  0.01180 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.09519 on 22 degrees of freedom
Multiple R-squared:  0.8755,    Adjusted R-squared:  0.8472 
F-statistic: 30.95 on 5 and 22 DF,  p-value: 2.991e-09
eq2<- lm(LOGSALES_2 ~ D2+D3+D4 +LOGA+LOGC)
summary(eq2)
Call:lm(formula = LOGSALES_2 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.16159 -0.04889  0.01773  0.05803  0.13346 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.69878    2.85860   1.294  0.20911    
D2           0.02273    0.05085   0.447  0.65925    
D3           0.16053    0.05095   3.151  0.00464 ** 
D4           0.49196    0.05209   9.444 3.38e-09 ***
LOGA        -0.55438    0.39162  -1.416  0.17088    
LOGC         0.55941    0.23940   2.337  0.02896 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.09478 on 22 degrees of freedom
Multiple R-squared:  0.8515,    Adjusted R-squared:  0.8177 
F-statistic: 25.22 on 5 and 22 DF,  p-value: 2.009e-08
eq3<- lm(LOGSALES_3 ~ D2+D3+D4+LOGA+LOGC)
summary(eq3)
Call:lm(formula = LOGSALES_3 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.15390 -0.03798  0.00795  0.05468  0.10389 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -8.34994    2.22231  -3.757  0.00109 ** 
D2           0.13628    0.03953   3.448  0.00230 ** 
D3           0.11866    0.03961   2.996  0.00666 ** 
D4           0.55283    0.04050  13.651 3.22e-12 ***
LOGA         1.96332    0.30445   6.449 1.73e-06 ***
LOGC         0.53377    0.18611   2.868  0.00894 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07368 on 22 degrees of freedom
Multiple R-squared:  0.9173,    Adjusted R-squared:  0.8985 
F-statistic: 48.81 on 5 and 22 DF,  p-value: 3.539e-11
eq4<- lm(LOGSALES_4 ~ D2+D3+D4+LOGA+LOGC)
summary(eq4)
Call:lm(formula = LOGSALES_4 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.110347 -0.027634 -0.005948  0.022954  0.092832 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.69548    1.68534   2.786  0.01077 *  
D2           0.24642    0.02998   8.220 3.75e-08 ***
D3           0.10502    0.03004   3.497  0.00204 ** 
D4           0.41367    0.03071  13.469 4.20e-12 ***
LOGA        -0.03128    0.23088  -0.135  0.89347    
LOGC        -0.11326    0.14114  -0.802  0.43087    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.05588 on 22 degrees of freedom
Multiple R-squared:  0.9102,    Adjusted R-squared:  0.8897 
F-statistic: 44.58 on 5 and 22 DF,  p-value: 8.719e-11
eq5<- lm(LOGSALES_5 ~ D2+D3+D4+LOGA+LOGC)
summary(eq5)
Call:lm(formula = LOGSALES_5 ~ D2 + D3 + D4 + LOGA + LOGC)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.133609 -0.043953 -0.009386  0.051460  0.140515 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.76576    2.21664   3.503  0.00201 ** 
D2           0.05113    0.03943   1.297  0.20815    
D3           0.10103    0.03951   2.557  0.01796 *  
D4           0.55536    0.04039  13.748  2.8e-12 ***
LOGA        -0.31963    0.30367  -1.053  0.30396    
LOGC        -0.31052    0.18564  -1.673  0.10855    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07349 on 22 degrees of freedom
Multiple R-squared:  0.9259,    Adjusted R-squared:  0.909 
F-statistic: 54.97 on 5 and 22 DF,  p-value: 1.075e-11                                          
