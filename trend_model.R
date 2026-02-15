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
