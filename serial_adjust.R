### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Model adjustments AR errors, clustered variances                       ###
### Example 5 24  Interest and Bond rates ###
xm511<- read.csv("xm511.csv",header=TRUE)
str(xm511)
attach(xm511)
panel01<- lm(DAAA~ DUS3MT)
summary(panel01)
### AR model with AR errors                                                ###
DUS3MT_lag<- c(NA, DUS3MT)
str(DUS3MT_lag)
DAAA_lag<- c(NA, lag(DAAA))
str(DAAA_lag)
model_lag<- lm(DAAA~ DUS3MT+DUS3MT_lag[1:600]+DAAA_lag[1:600])
summary(model_lag)
### Panel 2 AR model with AR errors (p.371)                                ###
lm(formula = DAAA ~ DUS3MT + DUS3MT_lag[1:600] + DAAA_lag[1:600])
Residuals:
     Min       1Q   Median       3Q      Max 
-0.61854 -0.06363 -0.00279  0.06788  0.98470 
Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)        0.004786   0.006724   0.712    0.477    
DUS3MT             0.252145   0.015019  16.788  < 2e-16 ***
DUS3MT_lag[1:600] -0.079633   0.017815  -4.470 9.37e-06 ***
DAAA_lag[1:600]    0.289874   0.040380   7.179 2.11e-12 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1644 on 595 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.4207,    Adjusted R-squared:  0.4178 
F-statistic:   144 on 3 and 595 DF,  p-value: < 2.2e-16
panel01_a<- lm(DAAA[2:600]~ DUS3MT[2:600])
model_lag_a<- lm(DAAA[2:600]~ DUS3MT[2:600]+DUS3MT_lag[2:600]+DAAA_lag[2:600])
anova(panel01_a, model_lag_a)
Analysis of Variance Table
Model 1: DAAA[2:600] ~ DUS3MT[2:600]
Model 2: DAAA[2:600] ~ DUS3MT[2:600] + DUS3MT_lag[2:600] + DAAA_lag[2:600]
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1    597 17.486                                  
2    595 16.087  2     1.399 25.872 1.683e-11 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
panel04<- lm(DAAA ~ DUS3MT + ar1_error[1:600])
summary(panel04)
### Panel 4 (p.370) lm(formula = DAAA ~ DUS3MT + ar1_error[1:600])         ###
Residuals:
       Min         1Q     Median         3Q        Max 
-3.283e-16 -3.850e-17 -2.310e-17 -5.100e-18  7.911e-15 
Coefficients:
                  Estimate Std. Error   t value Pr(>|t|)    
(Intercept)      6.393e-03  1.582e-17 4.041e+14   <2e-16 ***
DUS3MT           2.746e-01  3.316e-17 8.282e+15   <2e-16 ***
ar1_error[1:600] 3.619e+00  3.353e-16 1.079e+16   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 3.872e-16 on 596 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:      1,     Adjusted R-squared:      1 
F-statistic: 9.25e+31 on 2 and 596 DF,  p-value: < 2.2e-16

### Iterative two step method                                              ###
### Compare with the panel 5 (p.371)                                       ###
res<- resid(panel01)
acf(res)
res_1<- c(NA,res)
eq_res<- lm(res ~ res_1[1:600]-1)
summary(eq_res)
ar1_error<- fitted(eq_res)
acf(ar1_error)
Call:lm(formula = res ~ res_1[1:600] - 1)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.62867 -0.06437 -0.00241  0.06724  0.97222 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
res_1[1:600]  0.27630    0.03932   7.026  5.8e-12 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1644 on 598 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.07626,   Adjusted R-squared:  0.07471 
F-statistic: 49.37 on 1 and 598 DF,  p-value: 5.802e-12
y_co <- DAAA - 0.27630*DAAA_lag[1:600]
x_co <- DUS3MT - 0.27630*DUS3MT_lag[1:600]
co <- lm(y_co ~ x_co )
Call:
lm(formula = y_co ~ x_co)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.62232 -0.06372 -0.00242  0.06846  0.98880 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.004822   0.006709   0.719    0.473    
x_co        0.253305   0.014613  17.334   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1642 on 597 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.3348,    Adjusted R-squared:  0.3337 
F-statistic: 300.5 on 1 and 597 DF,  p-value: < 2.2e-16
summary(co)

### Example 5 25 (p.370)                                                   ###
xm520<- read.csv("xm520.csv", header =TRUE)
str(xm520)
attach(xm520)
f<- function(theta){
beta1 <- theta[1]
beta2 <- theta[2]
beta3 <- theta[3]
beta4 <- theta[4]
sigma <- theta[5]
N <- 48
mu <- beta1 + (beta2*TOTCONS^beta3)+beta4*AHSIZE
-N*0.5*log(2*pi) -N*0.5*log(sigma^2) - ((FRACFOOD - mu)^2/sigma^2)
}
library(maxLik)
m<- maxLik(f, start = c(0.45,-0.27,0.41,0.01,1))
summary(m)
### Compare with the panel 1 (p.373) Maximum Likelihood estimation          ###
Newton-Raphson maximisation, 8 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: 10599.18 
5  free parameters
Estimates:
       Estimate Std. error t value Pr(> t)    
[1,]  4.539e-01  7.174e-03   63.28  <2e-16 ***
[2,] -2.710e-01  7.068e-03  -38.34  <2e-16 ***
[3,]  4.126e-01  1.525e-02   27.06  <2e-16 ***
[4,]  1.696e-02  1.369e-04  123.86  <2e-16 ***
[5,]  2.431e-03  3.586e-05   67.81  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



### Package gslnls ###
library(gslnls)
### Regression Panel 2 Non-linear model ###
non_linear <- gsl_nls(fn = FRACFOOD ~ beta1 + (beta2*TOTCONS^beta3)+beta4*AHSIZE, data = xm520, 
start = c(beta1 = 0.4, beta2 = -0.2, beta3= 0.4,beta4=0.01))
summary(non_linear)
### Panel 1 FRACFOOD ~ beta1 + (beta2 * TOTCONS^beta3) + beta4 * AHSIZE    ###   
Parameters:
        Estimate Std. Error t value Pr(>|t|)    
beta1  0.4539213  0.0543000   8.360 1.24e-10 ***
beta2 -0.2710133  0.0534437  -5.071 7.62e-06 ***
beta3  0.4125870  0.1155453   3.571 0.000876 ***
beta4  0.0169607  0.0009913  17.110  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.01244 on 44 degrees of freedom
Number of iterations to convergence: 7 
Achieved convergence tolerance: 7.199e-14
res<- resid(non_linear)
res_1<- c(NA, res)
res_1<- res_1[1:48]
cor(res,res_1)
plot(res,res_1)
acf(res)

non_linear2 <- gsl_nls(fn = res ~beta1 + beta2*(TOTCONS^0.4125870)
             +beta3*(TOTCONS^0.4125870*log(TOTCONS))+beta4*AHSIZE
             + beta5*res_1, data = xm520, 
start = c(beta1 = 0.1, beta2 = -0.15, beta3= 0.07,beta4=0.01,beta5=0.1))
summary(non_linear2)

