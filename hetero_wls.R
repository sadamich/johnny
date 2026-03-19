### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 13 Bank wages p.331                                          ###
xm513<- read.csv("xm513.csv", header=TRUE)
str(xm513)
attach(xm513)
panel01<- lm(MEANLOGSAL~ MEANEDUC+GENDER+MINORITY+DUMJCAT2+DUMJCAT3)
summary(panel01)
### Panel 1 (p. 332) Call:lm(formula = MEANLOGSAL ~ MEANEDUC + GENDER      ###
###                                  + MINORITY + DUMJCAT2 + DUMJCAT3)     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.32369 -0.06412  0.00861  0.03348  0.40133 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.67344    0.14188  68.183  < 2e-16 ***
MEANEDUC     0.03359    0.01002   3.352  0.00318 ** 
GENDER       0.24952    0.07478   3.337  0.00329 ** 
MINORITY    -0.02444    0.06294  -0.388  0.70186    
DUMJCAT2     0.01953    0.09098   0.215  0.83224    
DUMJCAT3     0.67561    0.08466   7.980 1.21e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1575 on 20 degrees of freedom
Multiple R-squared:  0.8867,    Adjusted R-squared:  0.8584 
F-statistic:  31.3 on 5 and 20 DF,  p-value: 8.408e-09

coeftest(panel01, df = Inf, vcov = vcovHC(panel01, type = "HC1"))
### Panel 2 (p.332) z test of coefficients:                                ###
              Estimate Std. Error z value  Pr(>|z|)    
(Intercept)  9.6734398  0.1255414 77.0538 < 2.2e-16 ***
MEANEDUC     0.0335916  0.0096175  3.4928 0.0004781 ***
GENDER       0.2495224  0.0533516  4.6769 2.912e-06 ***
MINORITY    -0.0244436  0.0603894 -0.4048 0.6856493    
DUMJCAT2     0.0195258  0.1023407  0.1908 0.8486888    
DUMJCAT3     0.6756137  0.1048912  6.4411 1.186e-10 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


size<- as.numeric(GROUPSIZE)
v<- 1/sqrt(size)
eq_wls <- lm(MEANLOGSAL~ MEANEDUC+GENDER+MINORITY+DUMJCAT2+DUMJCAT3, weight = GROUPSIZE)
summary(eq_wls)
### Panel 3 (p.332) Call: lm(formula = MEANLOGSAL ~ MEANEDUC + GENDER +    ### 
###                            MINORITY + DUMJCAT2 +                       ###
    DUMJCAT3, weights = GROUPSIZE)
Weighted Residuals:
     Min       1Q   Median       3Q      Max 
-0.42041 -0.11854 -0.00198  0.21955  0.58734 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.586344   0.077396 123.860  < 2e-16 ***
MEANEDUC     0.043238   0.006123   7.061 7.58e-07 ***
GENDER       0.179823   0.029525   6.091 5.94e-06 ***
MINORITY    -0.074960   0.031581  -2.374   0.0277 *  
DUMJCAT2     0.166985   0.061281   2.725   0.0130 *  
DUMJCAT3     0.542568   0.042672  12.715 4.86e-11 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2747 on 20 degrees of freedom
Multiple R-squared:  0.974,     Adjusted R-squared:  0.9675 
F-statistic: 149.8 on 5 and 20 DF,  p-value: 3.887e-15








xm511<- read.csv("xm511.csv", header =TRUE)
str(xm511)
attach(xm511)
panel01<- lm(DAAA ~ DUS3MT)
summary(panel01)
### Panel 1 (p.341) Call:lm(formula = DAAA ~ DUS3MT)                       ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.70283 -0.07086 -0.00395  0.06571  1.06943 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.006393   0.006982   0.916     0.36    
DUS3MT      0.274585   0.014641  18.754   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.171 on 598 degrees of freedom
Multiple R-squared:  0.3703,    Adjusted R-squared:  0.3693 
F-statistic: 351.7 on 1 and 598 DF,  p-value: < 2.2e-16
res<- resid(panel01)
panel02<- lm(res^2 ~ DUM7599)
summary(panel02)
### Panel 2 (p.341) Call:lm(formula = res^2 ~ DUM7599)                     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.04857 -0.03664 -0.00933 -0.00225  1.09511 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.009719   0.004374   2.222   0.0267 *  
DUM7599     0.038850   0.006186   6.281 6.49e-10 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07576 on 598 degrees of freedom
Multiple R-squared:  0.06188,   Adjusted R-squared:  0.06031 
F-statistic: 39.45 on 1 and 598 DF,  p-value: 6.491e-10
v<- fitted(panel02)
DAAA_v <- DAAA/sqrt(v)
DUS3MT_v<- DUS3MT/sqrt(v)
panel03<- lm(DAAA ~ DUS3MT, weight = 1/v)
summary(panel03)
### Panel 3 (p.341) Call:lm(formula = DAAA ~ DUS3MT, weights = 1/v)        ###
Weighted Residuals:
    Min      1Q  Median      3Q     Max 
-4.7535 -0.5155 -0.0561  0.4383  5.0318 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.013384   0.005127    2.61  0.00927 ** 
DUS3MT      0.214989   0.014079   15.27  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9859 on 598 degrees of freedom
Multiple R-squared:  0.2805,    Adjusted R-squared:  0.2793 
F-statistic: 233.2 on 1 and 598 DF,  p-value: < 2.2e-16


library(maxLik)
loglik <- function(theta) {
 beta0 <- theta[1]
 beta1 <- theta[2]
 sigma <- theta[5]
 N <- nrow(xm511)
 mu <- beta0 + beta1*DUS3MT
 -1/2*N*log(2*pi) - 1/2*N*log(sigma^2) - 1/2*sum((DAAA -mu)^2/sigma^2)
 }
 m <- maxLik(loglik,start=c(beta0 = 0, beta1=0.2,sigma=1))
 summary(m)

res_1<- c(NA,res)
res_1<- res_1[1:600]
res_1sq<- res_1^2
eq_aux2<- lm(res^2 ~ res_1sq)
summary(eq_aux2)
### Panel 6 (p.342) Call:lm(formula = res^2 ~ res_1sq)                     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.23585 -0.02304 -0.02075 -0.00753  1.09447 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.023025   0.003336   6.901 1.32e-11 ***
res_1sq     0.211512   0.039997   5.288 1.74e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.0765 on 597 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.04475,   Adjusted R-squared:  0.04315 
F-statistic: 27.96 on 1 and 597 DF,  p-value: 1.736e-07
v1<- fitted(eq_aux2)
panel07<- lm(DAAA[2:600] ~ DUS3MT[2:600], weight = 1/v1)
summary(panel07)
### panel 7 (p.342) Call:lm(formula = DAAA[2:600] ~ DUS3MT[2:600],         ###
###                                                weights = 1/v1)         ###
Weighted Residuals:
    Min      1Q  Median      3Q     Max 
-3.9980 -0.4368 -0.0433  0.3874  4.7747 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.008738   0.006354   1.375     0.17    
DUS3MT[2:600] 0.284731   0.015628  18.219   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9577 on 597 degrees of freedom
Multiple R-squared:  0.3573,    Adjusted R-squared:  0.3562 
F-statistic: 331.9 on 1 and 597 DF,  p-value: < 2.2e-16