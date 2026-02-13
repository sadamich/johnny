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









