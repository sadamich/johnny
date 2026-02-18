### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
### Examplt 7 1 (p.532) Graphs of production                               ###
x<- ts(X, freq = 4, start = 1950)
plot(x, main = "Time", ylab = "Production")
y<- ts(Y, freq = 4, start = 1950)
### Trend of production and exhibit 7 1 b (p.533)                          ### 
plot(y, main = "Time", ylab = "log Production")
### Decompostion of production                                             ###
y_decom<- decompose(y)
plot(y_decom)
library(ggplot2)
library(seasonal)
seas(y) |> autoplot()
d4y_ts<- ts(D4Y, freq=4, start=1950)
### Stationary process and exhibit 7 1 c (p.533)                           ###
plot(d4y_ts, main= "Time series", ylab = "D4Y")
### Autocorrelation                                                        ###
lag.plot(Y, lags = 12, do.lines =FALSE)
acf(Y, 12)
lag.plot(D4Y[5:195], lags =12, do.lines =FALSE)
acf(D4Y[5:195], 12)
lag4<- c(NA,NA,NA,NA,Y)
growth<- D4Y/lag4[1:195]
growth_ts<- ts(growth,freq=4, start =1950) 
plot(growth_ts, main="Time", ylab="Growth rates")
### Linear trend model                                                     ###  
t <- 1:136
y_trend<- lm(Y[45:180] ~ t)
summary(y_trend)
res<- resid(y_trend)
res_ts<- ts(res, freq=4, start = 1961)
### Trend of residuals                                                     ###
plot(res_ts,main="Time series", ylab = "Residuals")
### Compare with panel1(p.591)                                             ###
### Linear forecast                                                        ###
f<- function(a, b){
result <- a + b*t
return(result)
}
fore<- f(3.7739920,0.0071399)
fore
plot(fore)

### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson, S.391 und S.384                          ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### And example 7 13 (p.589)                                                ###
y_61<- ts(Y[45:180], freq = 4, start = 1961)
plot(y_61, main = "Time", ylab = "log Production")
y_dfr<- data.frame(t = 137:150, yt = rep(0, 14))
y_prog<- predict(y_trend, newdata= y_dfr, freq=4, start = 1961)
plot(y_61, xlab = "Time", main = "Linear Trend", xlim= c(1961, 2010),
ylim=c(3.5, 5.0))
lines(ts(fitted(y_trend), freq= 4, start = 1961))
lines(ts(y_prog,freq= 4,start = 1995))
y_hw<- HoltWinters(y_61, alpha= 0.01)
y_hw_ts<- ts(c(fitted(y_hw)[,1], coef(y_hw)), freq=4, start = 1961)
lines(y_hw_ts, lty =1)

### ADF Test, compare with panel 1 and panel 5 (p.601)                      ###                                                                 ###
library(fUnitRoots)
unitrootTest(Y[45:180], lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)    
adfTest(Y[45:180], lags = 1, type = "ct", title = NULL, 
    description = NULL)
adfTest(Y[45:180], lags = 5, type = "ct", title = NULL, 
    description = NULL)
### ADF Test, compare with panel 1 and panel 5 (p.601)                      ###
library(tseries)
x<- Y[45:180]
adf.test(x, alternative = c("stationary", "explosive"),
         k = trunc((length(x)-1)^(1/3)))
adf.test(x, alternative = "stationary",
         k = trunc((length(x)-1)^(1/3)))

adf.test(x, alternative = "explosive",
         k = trunc((length(x)-1)^(1/3)))

### Exponential smoothing state space model                                 ###
library(forecast)
fit <- ets(y_61)
plot(forecast(fit))
### Differencing and detrending                                             ###
dy<- Y[45:180] - Y[44:179]
y_diff<- lm(dy ~ 1)
res_diff<- resid(y_diff)
res_diff_ts<- ts(res_diff, freq =4, start= 1961)
plot(res_diff_ts, main ="Residuals", xlim= c(1961, 2000))
### Panel4(p.591)              ###
summary(y_diff)
dy<- ts(dy, freq = 4, start = c(1961,2))
plot(dy, main = "Time", ylab = "log difference production")
dy_dfr<- data.frame(t = 137:150)
dy_prog<- predict(y_diff, newdata = dy_dfr, freq = 4, start= c(1961,2))
plot(dy, xlab ="Time", main= "differenced process", xlim = c(1961, 2010))
lines(ts(fitted(y_diff), freq= 4, start = 1961))
lines(ts(dy_prog, freq=4, start = 1995))
### Random walk model (package forecast)
library(forecast)
y_rw<- rw_model(Y, lag=1, drift = TRUE)
### Compare with exhibit 7 14 e (p.591)                                    ###
forecast(y_rw, h = 10) |> autoplot()
### Seasonal plot                                                          ###
ggseasonplot(y, col = rainbow(12), year.labels = TRUE)
ggseasonplot(y, year.labels = TRUE, continuous = TRUE)
seasonplot(y, col = rainbow(12), year.labels = TRUE)
### Seasonal adjustment                                                    ###
plot(y)
lines(seasadj(decompose(y, "multiplicative")), col = 4)
### Extract components from a time series decomposition                    ###
plot(y)
fit <- stl(y, s.window = "periodic")
lines(trendcycle(fit), col = "red")

library(ggplot2)
autoplot(
  cbind(
    Data = y,
    Seasonal = seasonal(fit),
    Trend = trendcycle(fit),
    Remainder = remainder(fit)
  ),
  facets = TRUE
) +
  labs(x = "Year", y = "")