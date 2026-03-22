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


### Example 5 14 Interest and Bond rates ###
xm511<- read.csv("xm511.csv",header =TRUE)
str(xm511)
attach(xm511)
panel01<- lm(DAAA~DUS3MT)
summary(panel01)
v<- DUS3MT^2
data<- cbind(DAAA, DUS3MT, v)
u<- c(6,7,14,23,168,170, 173,174,175,236,268,407,459,481,485,506,524)
DAAA<- DAAA[-u]
DUS3MT<- DUS3MT[-u]
v<- v[-u]
eq_wls<- lm(DAAA~ DUS3MT,weight = 1/v)
summary(eq_wls)
### Panel 2 (p. 333) Call:                                                 ###
lm(formula = DAAA ~ DUS3MT, weights = 1/v)
Weighted Residuals:
    Min      1Q  Median      3Q     Max 
-37.500  -0.374   0.032   0.512  48.500 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.002380   0.005143  -0.463   0.6437  
DUS3MT       0.262260   0.144280   1.818   0.0696 .
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 3.483 on 581 degrees of freedom
Multiple R-squared:  0.005655,  Adjusted R-squared:  0.003943 
F-statistic: 3.304 on 1 and 581 DF,  p-value: 0.06962


### Example 5 15 Bank wages ###
xm501<- read.csv("xm501.csv", header =TRUE)
attach(xm501)
eq_01<- lm(LOGSALARY ~ EDUC + GENDER + MINORITY + DUMJCAT2 + DUMJCAT3)
summary(eq_01)
### Panel 1 (p. 338) Call: lm(formula = LOGSALARY ~ EDUC +                 ###
###                           GENDER + MINORITY + DUMJCAT2 + DUMJCAT3)     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.44945 -0.12800 -0.01606  0.12080  0.87387 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.574694   0.054218 176.596  < 2e-16 ***
EDUC         0.044192   0.004285  10.313  < 2e-16 ***
GENDER       0.178340   0.020962   8.508 2.42e-16 ***
MINORITY    -0.074858   0.022459  -3.333 0.000927 ***
DUMJCAT2     0.170360   0.043494   3.917 0.000103 ***
DUMJCAT3     0.539075   0.030213  17.842  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1954 on 468 degrees of freedom
Multiple R-squared:  0.7608,    Adjusted R-squared:  0.7582 
F-statistic: 297.7 on 5 and 468 DF,  p-value: < 2.2e-16
res_eq_01<- resid(eq_01)
log01_res<-log(res_eq_01^2)
res_model<- lm(log01_res ~ DUMJCAT2 + DUMJCAT3)
summary(res_model)

### Panel 2 (p.338) Call:lm(formula = log01_res ~ DUMJCAT2 + DUMJCAT3)     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-10.0723  -0.9277   0.5556   1.5520   4.4636 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -4.7332     0.1235 -38.338   <2e-16 ***
DUMJCAT2     -0.2892     0.4692  -0.616    0.538    
DUMJCAT3      0.4605     0.2848   1.617    0.107    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 2.352 on 471 degrees of freedom
Multiple R-squared:  0.006882,  Adjusted R-squared:  0.002665 
F-statistic: 1.632 on 2 and 471 DF,  p-value: 0.1966
fit_res <- fitted(res_model)
v<- (exp(1.27- 4.7332 - 0.2892*DUMJCAT2+ 0.4605*DUMJCAT3))
fwls_sal<- lm(LOGSALARY ~ EDUC+GENDER+MINORITY+DUMJCAT2+DUMJCAT3, weight = 1/v)
summary(fwls_sal)
### Compare with the panel 3 (p.338) Call:                                 ###
### lm(formula = LOGSALARY ~ EDUC + GENDER + MINORITY + DUMJCAT2 +         ###
###                                        DUMJCAT3, weights = 1/v)        ###
Weighted Residuals:
    Min      1Q  Median      3Q     Max 
-2.6385 -0.7182 -0.0832  0.6833  4.9510 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.594903   0.052131 184.054  < 2e-16 ***
EDUC         0.042693   0.004123  10.356  < 2e-16 ***
GENDER       0.178160   0.020345   8.757  < 2e-16 ***
MINORITY    -0.078365   0.021330  -3.674 0.000266 ***
DUMJCAT2     0.167288   0.037542   4.456 1.05e-05 ***
DUMJCAT3     0.545052   0.032882  16.576  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.054 on 468 degrees of freedom
Multiple R-squared:  0.7166,    Adjusted R-squared:  0.7135 
F-statistic: 236.6 on 5 and 468 DF,  p-value: < 2.2e-16

### ML estimation with the variance model                                  ###
res<- resid(eq_01)
eq_res<- lm(log(res^2) ~ DUMJCAT2+DUMJCAT3)
summary(eq_res)
eq_ml<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
beta3<- theta[3]
beta4<- theta[4]
beta5<- theta[5]
beta6<- theta[6]
sigma<- theta[7]
gamma1<- theta[8]
gamma2<- theta[9]
gamma3<- theta[10]
N<- 474
mu<- beta1+ beta2*EDUC+beta3*GENDER+beta4*MINORITY+beta5*DUMJCAT2+beta6*DUMJCAT3

-N*0.5*log(2*pi)-N*0.5*log(sigma^2)- 0.5*((LOGSALARY -mu)^2/sigma^2)
(LOGSALARY - mu)^2 <- exp(gamma1+ gamma2*DUMJCAT2+gamma3*DUMJCAT3)

}

m<- maxLik(eq_ml, start= c(9.574694,0.044192,0.178340,-0.074858,0.170360,  
              0.539075,1,-4.7332 ,-0.2892 ,0.4605 ))
### Compare with the panel 4 (p.339)                                       ###
summary(m, eigentol=1e-15)

### Example 5 16 (p.341)                                                   ###
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
 sigma <- theta[3]
 gamma1<- theta[4]
 gamma2<- theta[5]
 N <- nrow(xm511)
 mu <- beta0 + beta1*DUS3MT
 e<- DAAA -mu
 e^2 <-  gamma1+ gamma2*DUM7599

 -1/2*N*log(2*pi) - 1/2*N*log(sigma^2) - 1/2*sum((e^2/sigma^2) 
 
 m <- maxLik(loglik,start=c(beta0 = 0.013384 , beta1=0.214989,sigma=1,
                            gamma1= 0.009719, gamma2=0.038850 ))
 summary(m)
### Clustered variances                                                    ###
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