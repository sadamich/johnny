### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 

### Example 7 8 Industrial Production  p.562 ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
detach(xm701)
D4Y_61<- D4Y[45:180]
### AR(2) Model ###
d4_1<- D4Y[44:179]
d4_2<- D4Y[43:178]

https://search.r-project.org/R/refmans/stats/html/filter.html

filter(x, filter, method = c("convolution", "recursive"),
       sides = 2, circular = FALSE, init)
ma <- stats::filter(D4Y_61,rep(1,3), method = "convolution",sides = 1)
ma1<- c(NA,ma)[1:136]
ma2<- c(NA,ma1)[1:136]
ma3<- c(NA,ma2)[1:136]
ma4<- c(NA,ma3)[1:136]
ma5<- c(NA,ma4)[1:136]
ar_25<- lm(D4Y_61 ~ d4_1+d4_2+ma1+ma2+ma3+ma4+ma5)
summary(ar_25)
???

ma1<- rep(1,1)
MA1<- filter(D4Y, ma1,sides=2)
ma2<- 1/2
MA2<- filter(D4Y, ma2,sides=2)
ma3<- rep(1/3, 3)
MA3<- filter(D4Y, ma3,sides=2)
ma4<- c(0.5, rep(1,3),0.5)/4
MA4<- filter(D4Y, ma4,sides=2)
ma5<- rep(1/5, 5)
MA5<- filter(D4Y, ma5,sides=2)

ar_25<- lm(D4Y_61 ~ d4_1+d4_2+MA1[45:180]+MA2[45:180]+MA3[45:180]
                      +MA4[45:180]+MA5[45:180])
summary(ar_25)
### Compare with the panel 1 (p.566) Call:                                 ###
### lm(formula = D4Y_61 ~ d4_1 + d4_2 + MA1 + MA2 + MA3 + MA4 + MA5)       ###
Residuals:
       Min         1Q     Median         3Q        Max 
-0.0133663 -0.0025947 -0.0002225  0.0020773  0.0188922 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.0012497  0.0006968  -1.793 0.075274 .  
d4_1        -0.1869944  0.0611056  -3.060 0.002695 ** 
d4_2        -0.4558829  0.0511866  -8.906 4.44e-15 ***
MA1          1.8765780  0.0992909  18.900  < 2e-16 ***
MA2          1.0850571  0.2886643   3.759 0.000258 ***
MA3         -2.4600261  0.3703917  -6.642 8.01e-10 ***
MA4          1.5194390  0.3155122   4.816 4.07e-06 ***
MA5         -0.3419825  0.1169408  -2.924 0.004083 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.005193 on 128 degrees of freedom
Multiple R-squared:  0.9894,    Adjusted R-squared:  0.9889 
F-statistic:  1714 on 7 and 128 DF,  p-value: < 2.2e-16

ar_2<- lm(D4Y_61 ~ d4_1+d4_2)
summary(ar_2)
### Exhibit 7 9 (p.563)Call: lm(formula = D4Y_61 ~ d4_1 + d4_2)            ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.067281 -0.007682  0.001154  0.011385  0.082473 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.007147   0.002161   3.307  0.00121 ** 
d4_1         1.332025   0.072094  18.476  < 2e-16 ***
d4_2        -0.545933   0.072174  -7.564 5.74e-12 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02096 on 133 degrees of freedom
Multiple R-squared:  0.8214,    Adjusted R-squared:  0.8187 
F-statistic: 305.8 on 2 and 133 DF,  p-value: < 2.2e-16
eq_ml<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
beta3<- theta[3]
sigma<- theta[4]
N<- 136
mu<- beta1+ beta2*d4_1+beta3*d4_2
-N*0.5*log(2*pi) - N*0.5*log(sigma^2) - 0.5*((D4Y_61 -mu)^2/sigma^2)
}
library(maxLik)
m<- maxLik(eq_ml, start=c(0.007147,1.332025,-0.545933,1))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 2 iterations
Return code 3: Last step could not find a value above the current.
Boundary of parameter space?  
Consider switching to a more robust optimisation method temporarily.
Log-Likelihood: -6884.248 
4  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)
[1,]  0.006839        Inf       0       1
[2,]  1.331714        Inf       0       1
[3,] -0.546244        Inf       0       1
[4,] -0.578833        Inf       0       1
--------------------------------------------
grad<- gradient(m)
[1] 1.330517e-01 4.784148e-03 4.750426e-03 3.195365e+04
hess<- hessian(m)
      [,1]        [,2]        [,3]         [,4]
[1,] -404.7251423 -11.8234311 -11.8234311 9.094947e-01
[2,]  -11.8234311  -0.9094947  -0.9094947 9.094947e-01
[3,]  -11.8234311  -0.9094947  -0.9094947 9.094947e-01
[4,]    0.9094947   0.9094947   0.9094947 5.520269e+04

### Compare with exhibit 7 10, panel 1 (p.566)                             ### 
library(tseries)
arma25<- arma(D4Y_61, order=c(2,5))
summary(arma25)
### Residuals                                                              ###
res <- resid(ar_2)
res_ts <- ts(res, freq =4, start = 1961)
### Exhibit 7 11 (a and b)(p. 573)                                         ###
plot(res_ts, main="Time series", ylab ="Reduals")
hist(res_ts)
acf(res_ts)
d4_fit<- fitted(ar_2)
d4_fit_ts<- ts(d4_fit, freq= 4, start =1961)
### Exhibit 7.9 (p.563) Call:lm(formula = D4Y_61 ~ d4_1 + d4_2)            ###
predict(ar_2)[1:100]
plot(predict(ar_2)[1:100])
### https://search.r-project.org/R/refmans/stats/html/predict.arima.html   ###
### predict.Arima {stats}                                                  ###
(fit<- arima(D4Y_61, order = c(2,0,0)))
predict(fit, n.ahead = 5)
library(forecast)
D4Y_61 |>
  Arima(order = c(2, 0, 0)) |>
  forecast(h = 10) |>
  autoplot()
fit <- auto.arima(D4Y_61)
### ARMA automatich ordering ###
plot(forecast(fit, h = 10))

findfrequency(D4Y_61)

fit <- ets(D4Y_61)
plot(D4Y_61)
lines(fitted(fit), col = "red")
lines(fitted(fit, h = 2), col = "green")
lines(fitted(fit, h = 3), col = "blue")
legend("topleft", legend = paste("h =", 1:3), col = 2:4, lty = 1)
### The time series of residuals, panel(a), p.573                          ###
### The histgram and summary of residuals, panel(b), p.573                 ###
plot(res_ts, main = " Time series ", ylab = "Residuals") 
hist(res)
summary(res)
### Correlogram of residuals of the model AR(2), panel3, p.573             ###
Acf(res, main = "ACF")
### ARCH LM test ###
res_lag <- c(NA, res)
res_lag2<- c(NA, res_lag)
res_lag3<- c(NA, res_lag2)
res_lag4<- c(NA, res_lag3)
panel02 <- lm(res ~ d4_1+ d4_2 + res_lag[1:136]
+ res_lag2[1:136]+res_lag3[1:136]+ res_lag4[1:136])
summary(panel02)
### Compare with the exhibit 7. 11 Panel 3 and Panel 4 (p.573)
LM = n*R^2 = 
LM = (n*g*F)/(n-k+g*F)
### Asymptotic LM ~ F ###
### STAR model, compare with panel 1 (p.619)                               ###
### TAR Model panel 2 (p.619)                                              ###
dum<- ifelse(d4_1 >0, 1, 0)
ar2_tar<- lm(D4Y_61 ~ d4_1+d4_2+ dum*(d4_1+d4_2))
summary(ar2_tar)
Call: lm(formula = D4Y_61 ~ d4_1 + d4_2 + dum * (d4_1 + d4_2))
Residuals:
      Min        1Q    Median        3Q       Max 
-0.066517 -0.010748 -0.000278  0.010922  0.082829 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.006937   0.005423  -1.279   0.2031    
d4_1         1.172922   0.138174   8.489 4.10e-14 ***
d4_2        -0.714031   0.112871  -6.326 3.73e-09 ***
dum          0.014694   0.006739   2.180   0.0310 *  
d4_1:dum     0.092095   0.170993   0.539   0.5911    
d4_2:dum     0.247076   0.144471   1.710   0.0896 .  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02038 on 130 degrees of freedom
Multiple R-squared:  0.8349,    Adjusted R-squared:  0.8286 
F-statistic: 131.5 on 5 and 130 DF,  p-value: < 2.2e-16
res_tar<- resid(ar2_tar)
### Exhibit 7 22 c (p.619)                                                 ###
hist(res_tar)
### STAR Model compare with panel 1 (p.619)
library(gslnls)
### Regression Panel 2 Non-linear model ###
d4_star <- gsl_nls(fn = D4Y_61 ~ beta_1 + beta_2*d4_1 + beta_3*d4_2 +
 (beta_4+ beta_5*d4_1 + beta_6*d4_2)/(1 + exp(-beta_7*(d4_1 - beta_8))),
data=xm701,
start = c(beta_1 = 0, beta_2 = 1, beta_3 = 0,
beta_4 = 0, beta_5 = 0, beta_6 = 0, beta_7 = NA, beta_8 = 0)
)
coef(d4_star)
summary(d4_star)
predict(d4_star, interval = "prediction")
Formula: D4Y_61 ~ beta_1 + beta_2 * d4_1 + beta_3 * d4_2 + (beta_4 + beta_5 * 
    d4_1 + beta_6 * d4_2)/(1 + exp(-beta_7 * (d4_1 - beta_8)))
Parameters:
        Estimate Std. Error t value Pr(>|t|)    
beta_1 -0.003133   0.010371  -0.302    0.763    
beta_2  1.247395   0.155029   8.046 5.02e-13 ***
beta_3 -0.725212   0.135126  -5.367 3.63e-07 ***
beta_4  0.018507   0.023843   0.776    0.439    
beta_5 -0.110143   0.260573  -0.423    0.673    
beta_6  0.300250   0.188442   1.593    0.114    
beta_7 77.685308  93.750658   0.829    0.409    
beta_8  0.014620   0.021987   0.665    0.507    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.02074 on 128 degrees of freedom
Number of iterations till stop: 100 
Achieved convergence tolerance: 1.839e-05
Reason stopped: exceeded max number of iterations

### ARCH                                                                   ###
library(tseries)
d4_arch<- garch(d4_fit_ts, order=c(0,1))
summary(d4_arch)
plot(d4_arch)
d4_ar2<- arma(D4Y_61, order=c(2,0))
summary(d4_ar2)
d4_fit<- fitted(d4_ar2)
garch(d4_fit[3:136], order= c(0,1))
summary(garch(d4_fit[3:136], order= c(0,1)))

library(tseries)
eq1<- arma(D4Y_61, order =c(2,0))
summary(eq1)
Call:
arma(x = D4Y_61, order = c(2, 0))
Model:ARMA(2,0)
Residuals:
       Min         1Q     Median         3Q        Max 
-0.0680121 -0.0078512  0.0004884  0.0109455  0.0658293 
Coefficient(s):
           Estimate  Std. Error  t value Pr(>|t|)    
ar1        1.391787    0.066921   20.797  < 2e-16 ***
ar2       -0.588858    0.065668   -8.967  < 2e-16 ***
intercept  0.006436    0.001945    3.309 0.000935 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Fit: sigma^2 estimated as 0.0003518,  Conditional Sum-of-Squares = 0.05,  
AIC = -689.58
res_ar2<- resid(eq1)
res_ar2<- na.omit(res_ar2)
garch(res_ar2, order = c(0,1))

### ML estimation AR(2) ARCH (1)                                               ###
eq_arch <- function(theta) {
 beta0 <- theta[1]
 beta1 <- theta[2]
 beta2 <- theta[3]
 gamma1<- theta[4] 
 gamma2<- theta[5]
 N <- 136
 mu <- beta0 + beta1*d4_1+beta2*d4_2
 e <- D4Y_61 - mu
 e_1<- c(NA,e)
 h<- gamma1+gamma2*e_1^2
 -0.5*N*log(2*pi) - 0.5*N*log(h) - 0.5*sum((D4Y_61 - mu)^2/h) 
}
 
m_arch <- maxLik(eq_arch,start=c(0.007,1.3,0,0,1))
summary(m_arch)???







