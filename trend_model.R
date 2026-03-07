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
### Panel 1 (p.591) Call: lm(formula = Y[45:180] ~ t)                      ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.239072 -0.051494  0.007656  0.045814  0.132270 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 3.7739920  0.0124815  302.37   <2e-16 ***
t           0.0071399  0.0001581   45.16   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07238 on 134 degrees of freedom
Multiple R-squared:  0.9384,    Adjusted R-squared:  0.9379 
F-statistic:  2040 on 1 and 134 DF, p-value: < 2.2e-16

res<- resid(y_trend)
res_ts<- ts(res, freq=4, start = 1961)
### Trend of residuals                                                     ###
plot(res_ts,main="Time series", ylab = "Residuals")
### Compare with Exhibit 7 14 (c) (p.591)                                  ###           ###
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
summary(y_diff)
### Panel 4 Random walk Model: Call: lm(formula = dy ~ 1)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.096148 -0.011829  0.001002  0.014492  0.048904 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.008400   0.001793   4.686 6.71e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.0209 on 135 degrees of freedom
res_rw<- resid(y_diff)
res_rw<- ts(res_rw, freq = 4, start = 1961)
### Exhibit 7 14 (f) (p.591)                                               ###
plot(res_rw, main = "Time series", ylab= "Residuals")
dy<- ts(dy, freq = 4, start = c(1961,2))
plot(dy, main = "Time", ylab = "log difference production")
dy_dfr<- data.frame(t = 137:150)
dy_prog<- predict(y_diff, newdata = dy_dfr, freq = 4, start= c(1961,2))
plot(dy, xlab ="Time", main= "differenced process", xlim = c(1961, 2010))
lines(ts(fitted(y_diff), freq= 4, start = 1961))
lines(ts(dy_prog, freq=4, start = 1995))
### Random walk model 
library(forecast)
y_rw<- rw_model(Y, lag=1, drift = TRUE)
### Compare with exhibit 7 14 e (p.591)                                    ###
forecast(y_rw, h = 10) |> autoplot()

str(dy)
y_1<- Y[44:179]
str(y_1)
eq_adf<- lm(dy~y_1+t)
summary(eq_adf)
Call: lm(formula = dy ~ y_1 + t)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.094435 -0.009790  0.002517  0.013207  0.043876 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.2610134  0.0884636   2.951  0.00375 **
y_1         -0.0657626  0.0235076  -2.798  0.00591 **
t            0.0003969  0.0001753   2.263  0.02523 * 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02025 on 133 degrees of freedom
Multiple R-squared:  0.07587,   Adjusted R-squared:  0.06198 
F-statistic:  5.46 on 2 and 133 DF,  p-value  0.005262
### Autocorrelation panel 3 (p.601)                                        ###
res<- resid(eq_adf)
acf(res)
anova(eq_adf)
Analysis of Variance Table
Response: dy
           Df   Sum Sq    Mean Sq F value  Pr(>F)  
y_1         1 0.002376 0.00237620  5.7966 0.01743 *
t           1 0.002100 0.00210014  5.1232 0.02523 *
Residuals 133 0.054521 0.00040993                  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
eq_adf2<- lm(dy~1)
summary(eq_adf2)
anova(eq_adf,eq_adf2)
Analysis of Variance Table
Model 1: dy ~ y_1 + t
Model 2: dy ~ 1
  Res.Df      RSS Df  Sum of Sq      F   Pr(>F)   
1    133 0.054521                                 
2    135 0.058997 -2 -0.0044763 5.4599 0.005262 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 

str(Y)
Dy<- Y[2:195]-Y[1:194]
str(Dy)
Ddy<- Dy[45:180] - Ddy[44:179]
str(Ddy)
Ddy1<- Ddy[44:179] - Ddy[43:178]
Ddy2<- Ddy[43:178] - Ddy[42:177]
Ddy3<- Ddy[42:177] - Ddy[41:176]
Ddy4<- Ddy[41:176] - Ddy[40:175]
Ddy5<- Ddy[40:175] - Ddy[39:174]
eq_d_adf<- lm(Ddy ~ y_1 + Ddy1+Ddy2+Ddy3+Ddy4+Ddy5 + t)
summary(eq_d_adf)
### Seasonality                                                            ###
### ADF Test for checking the seasonality: H0 is I(2)                      ###
### ADF Test, compare with panel 1,panel 3 and panel 4 (p.609)             ###                                                                 ###
library(fUnitRoots)
DY <- Y[45:180] -Y[44:179]
unitrootTest(DY, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)    
adfTest(DY, lags = 1, type = "ct", title = NULL, 
    description = NULL)
adfTest(DY, lags = 5, type = "ct", title = NULL, 
    description = NULL)
### H0 (I(2)) is rejected                                                  ###
unitrootTest(YEARSUMY[45:180], lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)    
adfTest(YEARSUMY[45:180], lags = 1, type = "ct", title = NULL, 
    description = NULL)
adfTest(YEARSUMY[45:180], lags = 4, type = "ct", title = NULL)
d_YEAR <- YEARSUMY[45:180] - YEARSUMY[44:179]
adfTest(d_YEAR, lags = 4, type = "c", title = NULL)

### Seasonal plot                                                          ###
ggseasonplot(y, col = rainbow(12), year.labels = TRUE)
ggseasonplot(y, year.labels = TRUE, continuous = TRUE)
seasonplot(y, col = rainbow(12), year.labels = TRUE)
### Seasonal adjustment                                                    ###
library(forecast)
plot(y)
### Seasonal adjustment, compare with exhibit 7 20 a (p.610)               ###
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