### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 22 Interest and Bond rates (p.365)                           ###
xm511 <- read.csv("xm511.csv", header= TRUE)
str(xm511)
attach(xm511)
eq00<- lm(DAAA~DUS3MT)
summary(eq00)
res<- resid(eq00)
### Panel 2 (p.366)                                                        ###
acf(res)
res_lag<- c(NA, res)
str(res_lag)
res_lag<- res_lag[1:600]
panel03<- lm(res ~ DUS3MT + res_lag)
summary(panel03)
### Panel 3 LM test (p.366) Call:lm(formula = res ~ DUS3MT + res_lag)      ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.61854 -0.06363 -0.00279  0.06788  0.98470 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.0002455  0.0067136   0.037    0.971    
DUS3MT      -0.0224507  0.0144081  -1.558    0.120    
res_lag      0.2898803  0.0402649   7.199 1.83e-12 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1643 on 596 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.08001,   Adjusted R-squared:  0.07692 
F-statistic: 25.92 on 2 and 596 DF,  p-value: 1.614e-11
LM= 600*0.08001= 48.006 (H0 is rejectet) 

res_lag2<- c(NA, res_lag)
res_lag2<- res_lag2[1:600]
panel04<- lm(res ~ DUS3MT + res_lag+ res_lag2)
summary(panel04)
### Panel 4 LM test (p.366):lm(formula = res ~ DUS3MT + res_lag + res_lag2)###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.56807 -0.06727 -0.00448  0.06435  0.95698 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.0003384  0.0066280   0.051   0.9593    
DUS3MT      -0.0290521  0.0142952  -2.032   0.0426 *  
res_lag      0.3425900  0.0415649   8.242 1.08e-15 ***
res_lag2    -0.1750631  0.0406845  -4.303 1.97e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1621 on 594 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared:  0.1078,    Adjusted R-squared:  0.1033 
F-statistic: 23.93 on 3 and 594 DF,  p-value: 1.25e-14
LM = n R^2 = 600*0.1078=64.68 (H0 is rejectet)  