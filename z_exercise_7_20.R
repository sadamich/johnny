### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 20 (p.718)                                                  ###
xr720 <- read.csv("xr720.csv", header =TRUE)
str(xr720)
attach(xr720)
### a Graphical interpretation                                             ###
### Exponential trend                                                      ###
Y <- USA
Y <- ts(Y, freq = 1, start = 1870)
plot(Y, main = "Time series", ylab = "GNP")
### Linear trend                                                           ###
y <- LOGUSA
y <- ts(y, freq = 1, start = 1870)
plot(y, main = "Time series", ylab = "GNP")
### b Three intervals                                                      ###
OBS
y_1<- y[1:60]
y_1<- ts(y_1, freq = 1, start = 1870)
plot(y_1, main = "1870 - 1929", ylab = "GNP") 
### World wars and restruction after wars                                  ###
y_2<- y[31:80]
y_2<- ts(y_2, freq = 1, start = 1900)
plot(y_2, main = "1900 - 1949", ylab = "GNP")  
y_3<- y[81:124]
y_3<- ts(y_3, freq = 1, start = 1950)
plot(y_3, main = "1950 - 1993", ylab = "GNP") 
### Problem c  1950-1989                                                   ###
OBS
yc<- y[81:120] 
yc<- ts(yc, freq =1, start = 1950)
plot(yc, main = "Time series", ylab = "GNP")
yc_1<- y[80:119]  
t<- 1:40
eq_t <- lm(yc ~ t)
summary(eq_t)   
### Problem (c and d) Trend model Call:lm(formula = yc ~ t)                ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.061170 -0.025469 -0.001332  0.025568  0.069200 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 14.206801   0.012068  1177.2   <2e-16 ***
t            0.033240   0.000513    64.8   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.03745 on 38 degrees of freedom
Multiple R-squared:  0.991,     Adjusted R-squared:  0.9908 
F-statistic:  4199 on 1 and 38 DF,  p-value: < 2.2e-16
fit<- fitted(eq_t)
fit<- ts(fit, freq=1, start = 1950)
lines(fit, main = "Time series", ylab = "fitted value",,col ="red",add=TRUE)
library(forecast)
eq_prog<- ets(yc)
plot(forecast(eq_prog))
eq_rw<- lm(yc ~ yc_1)
summary(eq_rw)
### Problem (c and d) Random walk model Call:lm(formula = yc ~ yc_1        ### 
Residuals:
      Min        1Q    Median        3Q       Max 
-0.049915 -0.016884  0.003002  0.019721  0.048022 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.2811     0.1500   1.874   0.0687 .  
yc_1          0.9834     0.0101  97.415   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02497 on 38 degrees of freedom
Multiple R-squared:  0.996,     Adjusted R-squared:  0.9959 
F-statistic:  9490 on 1 and 38 DF,  p-value: < 2.2e-16         

model <- rw_model(yc)
forecast(model, h = 10) |> autoplot()

### Problem (e) ARMA Model
eq_arma<- Arima(yc, order = c(2,0,5))
summary(eq_arma)
Series: yc 
ARIMA(2,0,5) with non-zero mean 

Coefficients:
         ar1      ar2      ma1     ma2     ma3     ma4     ma5     mean
      1.7183  -0.7229  -0.3840  0.0182  0.0764  0.5243  0.1972  14.7588
s.e.  0.1626   0.1633   0.2656  0.1574  0.1917  0.1963  0.2045   0.6171

sigma^2 = 0.0007998:  log likelihood = 84.28
AIC=-150.56   AICc=-144.56   BIC=-135.36

Training set error measures:
                      ME       RMSE        MAE        MPE      MAPE      MASE
Training set 0.005791422 0.02529541 0.02097147 0.03864299 0.1409333 0.5822993
                   ACF1
Training set -0.1563415
library(tseries)
adf.test(yc)    
 Augmented Dickey-Fuller Test
data:  yc
Dickey-Fuller = -1.5334, Lag order = 3, p-value = 0.7566
alternative hypothesis: stationary    
dy<- yc - yc_1
adf.test(dy) 
 Augmented Dickey-Fuller Test

data:  dy
Dickey-Fuller = -3.5669, Lag order = 3, p-value = 0.04822
alternative hypothesis: stationary
### If yc was I(1), it was random walk.                                    ###
library(forecast)
model <- rw_model(yc)
forecast(model, h = 10) |> autoplot()       

### Cointegrations                                                         ###
y_uk<- LOGUK
y_ger<- LOGGER
y_jp<- LOGJAP
adf.test(y_uk)
Augmented Dickey-Fuller Test
data:  y_uk
Dickey-Fuller = -1.5691, Lag order = 4, p-value = 0.7556
alternative hypothesis: stationary
adf.test(y_ger)
 Augmented Dickey-Fuller Test
data:  y_ger
Dickey-Fuller = -2.3551, Lag order = 4, p-value = 0.429
alternative hypothesis: stationary
adf.test(y_jp)
y_jp<- na.omit(y_jp)
adf.test(y_jp)  
 Augmented Dickey-Fuller Test
data:  y_jp
Dickey-Fuller = -1.7525, Lag order = 4, p-value = 0.6793
alternative hypothesis: stationary  

attach(xr720)
y_jp<- LOGJAP
gnp<- data.frame(y[16:124],y_uk[16:124],y_ger[16:124],y_jp[16:124])
library(tsDyn)
gnp_co<- VECM(
  data = gnp,
  lag=2,
  r = 3,
  include = "const",
  beta = NULL,
  estim = "ML",
  LRinclude = "both",
  exogen = NULL
)

  ECT1        ECT2        ECT3 y.16.124. -1
Equation y.16.124.     -0.13167573  0.10937622 -0.03756498   0.15175516
Equation y_uk.16.124.  -0.07044076 -0.13156313  0.06990220   0.03354203
Equation y_ger.16.124. -0.16682959  0.03666496 -0.11161956   0.04313404
Equation y_jp.16.124.  -0.24222616  0.20787885  0.09063559  -0.15815543
                       y_uk.16.124. -1 y_ger.16.124. -1 y_jp.16.124. -1
Equation y.16.124.          0.40404515       0.11065394     0.148036220
Equation y_uk.16.124.       0.28609248      -0.06142459     0.073340843
Equation y_ger.16.124.     -0.01271546       0.09356375     0.557187846
Equation y_jp.16.124.       0.43059647      -0.04512669     0.007652393
                       y.16.124. -2 y_uk.16.124. -2 y_ger.16.124. -2
Equation y.16.124.       0.08361507     -0.02758210     0.0516055685
Equation y_uk.16.124.   -0.06854700      0.03439935     0.0006157755
Equation y_ger.16.124.   0.06738399      0.12199422    -0.0819811683
Equation y_jp.16.124.   -0.36426479     -0.08953156     0.0272199602
                       y_jp.16.124. -2
Equation y.16.124.        -0.097222854
Equation y_uk.16.124.      0.002130671
Equation y_ger.16.124.     0.009761899
Equation y_jp.16.124.      0.016623595
Warning message:
In lineVar(data, lag, r = r, include = include, model = "VECM",  :
  When `LRinclude` is either 'const' or 'both', `include` can only be `none`.
  Setting include='none'.
coefA(gnp_co)
coefB(gnp_co)
coefPI(gnp_co)

