### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 26 Industrial production ###
xm526<- read.csv("xm526.csv", header = TRUE)
str(xm526)
attach(xm526)
log_IP<- log(IP)
t<- 1:195
panel01<- lm(log(IP) ~ t)
summary(panel01)
### Panel 1 (p.375) Call:lm(formula = log(IP) ~ t)                         ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.21506 -0.06316 -0.01191  0.06406  0.17922 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 3.3135599  0.0122344  270.84   <2e-16 ***
t           0.0081965  0.0001083   75.72   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.08509 on 193 degrees of freedom
Multiple R-squared:  0.9674,    Adjusted R-squared:  0.9673 
F-statistic:  5733 on 1 and 193 DF,  p-value: < 2.2e-1
res<- resid(panel01)
res_1<- c(NA, res)[1:195]
res_2<- c(NA, res_1)[1:195]
acf(res)
eq_lm<- lm(res ~ t+ res_1+res_2)
summary(eq_lm)
### Compare with the panel 3 (p.375) lm(formula = res ~ t + res_1 + res_2) ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.081310 -0.015832  0.001028  0.015143  0.069279 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.777e-03  3.487e-03   0.796  0.42685    
t           -2.210e-05  3.068e-05  -0.720  0.47221    
res_1        1.145e+00  7.061e-02  16.218  < 2e-16 ***
res_2       -2.022e-01  6.943e-02  -2.912  0.00402 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02365 on 189 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared:  0.9203,    Adjusted R-squared:  0.9191 
F-statistic: 727.8 on 3 and 189 DF,  p-value: < 2.2e-16
LM= 195* 0.9203 = [1] 179.4585 (H0 is rejectet) 


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

dy<- Y[45:180] - Y[44:179]
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
### Panel 2 (p.601) Analysis of Variance Table                             ###
Model 1: dy ~ y_1 + t
Model 2: dy ~ 1
  Res.Df      RSS Df  Sum of Sq      F   Pr(>F)   
1    133 0.054521                                 
2    135 0.058997 -2 -0.0044763 5.4599 0.005262 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 

dy<- Y[45:180] - Y[44:179]
dy1<- Y[44:179] - Y[43:178]
dy2<- Y[43:178] - Y[42:177]
dy3<- Y[42:177] - Y[41:176]
dy4<- Y[41:176] - Y[40:175]
dy5<- Y[40:175] - Y[39:174]
eq_d_adf<- lm(dy ~ y_1 + dy1+dy2+dy3+dy4+dy5 + t)
summary(eq_d_adf)
### Panel 5(p. 601)Call: lm(formula = dy ~ y_1 + dy1 + dy2 + dy3           ###
###                                        + dy4 + dy5 + t)                ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.064883 -0.007754  0.001295  0.009187  0.053379 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.2468602  0.0818114   3.017 0.003076 ** 
y_1         -0.0629964  0.0218799  -2.879 0.004675 ** 
dy1          0.2672903  0.0803538   3.326 0.001148 ** 
dy2          0.1118044  0.0778542   1.436 0.153421    
dy3         -0.1501269  0.0768446  -1.954 0.052925 .  
dy4          0.3456369  0.0763037   4.530 1.34e-05 ***
dy5         -0.2897889  0.0791537  -3.661 0.000366 ***
t            0.0003974  0.0001657   2.398 0.017922 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.01777 on 128 degrees of freedom
Multiple R-squared:  0.3148,    Adjusted R-squared:  0.2773 
F-statistic: 8.402 on 7 and 128 DF,  p-value: 1.979e-08

anova(eq_d_adf)
Analysis of Variance Table
Response: dy
           Df   Sum Sq   Mean Sq F value    Pr(>F)    
y_1         1 0.002376 0.0023762  7.5242 0.0069612 ** 
dy1         1 0.000626 0.0006258  1.9815 0.1616580    
dy2         1 0.002115 0.0021149  6.6967 0.0107750 *  
dy3         1 0.001973 0.0019727  6.2464 0.0137104 *  
dy4         1 0.003922 0.0039223 12.4199 0.0005898 ***
dy5         1 0.005745 0.0057453 18.1922 3.849e-05 ***
t           1 0.001816 0.0018162  5.7511 0.0179218 *  
Residuals 128 0.040424 0.0003158                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
eq_d_adf2<- lm(dy ~ dy1+dy2+dy3+dy4+dy5)
anova(eq_d_adf, eq_d_adf2)
### Panel 6 (p.601) Analysis of Variance Table                            ###
Model 1: dy ~ y_1 + dy1 + dy2 + dy3 + dy4 + dy5 + t
Model 2: dy ~ dy1 + dy2 + dy3 + dy4 + dy5
  Res.Df      RSS Df  Sum of Sq      F   Pr(>F)   
1    128 0.040424                                 
2    130 0.043893 -2 -0.0034695 5.4931 0.005144 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 

### Panel 7 (p.601)                                                       ###
res<- resid(eq_d_adf)
acf(res)

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
### Panel 1 (p.609)
dy<- Y[45:180] - Y[44:179]
dy_1<- Y[44:179] - Y[43:178]
Ddy<- dy[2:136] - dy[1:135]
Ddy1<- c(NA,Ddy)[1:135]
Ddy2<- c(NA,Ddy1)[1:135]
Ddy3<- c(NA,Ddy2)[1:135]
Ddy4<- c(NA,Ddy3)[1:135]
eq_adf_s<- lm(Ddy ~ dy_1[1:135] + Ddy1 + Ddy2 + Ddy3 + Ddy4)
summary(eq_adf_s)
acf(dy)
str(YEARSUMY)
d_YEAR <- YEARSUMY[45:180] - YEARSUMY[44:179]
y_year_1<- YEARSUMY[44:179]
d_YEAR1<- c(NA,d_YEAR)[1:136]
d_YEAR2<- c(NA,d_YEAR1)[1:136]
d_YEAR3<- c(NA,d_YEAR2)[1:136]
d_YEAR4<- c(NA,d_YEAR3)[1:136]
t<- 1:136
eq_s<- lm(d_YEAR ~ y_year_1 + d_YEAR1 + d_YEAR2 + d_YEAR3 + d_YEAR4 + t)
summary(eq_s)
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