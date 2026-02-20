### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 

### Example 7 8 Industrial Production  p.562 ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
D4Y_61<- D4Y[45:180]
### AR(2) Model ###
d4_1<- D4Y[44:179]
d4_2<- D4Y[43:178]
ar_2<- lm(D4Y_61 ~ d4_1+d4_2)
summary(ar_2)
### Residuals                                                              ###
res <- resid(ar_2)
res_ts <- ts(res, freq =4, start = 1961)
plot(res_ts, main="Time series", ylab ="Reduals")
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
dum<- ifelse(d4_1 >0, "1", "0")
dum<- as.numeric(dum)
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
 (beta_4+ beta_5*d4_1 + beta_6*d4_2)/(1+exp(-beta_7)*(d4_1 - beta_8)),
data=xm701,start = c(beta_1=NA, beta_2=NA, beta_3=NA,
beta_4=NA, beta_5=NA, beta_6=NA, beta_7=NA, beta_8=NA))
coef(d4_star)
summary(d4_star)

library(tsDyn)
star(D4Y_61, m=2, noRegimes, d = 1, steps = 1,  rob = FALSE )

### Self Threshold Autoregressive model, compare with panel 2 (p.619)      ###
setar(D4Y_61, m=2, mTh=c(0,1))

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








