### Source: https://finance.yahoo.co.jp/                                     ###
aktien<- read.csv("aktien.csv", header=TRUE)
str(aktien)
attach(aktien)
str(cyber)
x<- cyber
acf(x)
plot(x, main= "Time series",typ="l")
x<- ts(x, freq = 5, start =1)
plot(decompose(x))
t<- 1:25
kurs<- lm(x ~ t)
summary(kurs)
x_fit<- fitted(kurs)
lines(x_fit,col = "red", add=TRUE)
res<- resid(kurs)
acf(res)
res_sq<- res^2
acf(res_sq)
res<<- ts(res,freq=5, start =1)
plot(res, main = "Time series", ylab= "Residuals")
### Estimation of trend model                                              ###
Call: lm(formula = x ~ t)
Residuals:
    Min      1Q  Median      3Q     Max 
-19.615  -8.766  -3.125   6.668  22.725 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 218.8600     5.0358   43.46  < 2e-16 ***
t             3.6415     0.3387   10.75 1.92e-10 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 12.21 on 23 degrees of freedom
Multiple R-squared:  0.834,     Adjusted R-squared:  0.8268 
F-statistic: 115.6 on 1 and 23 DF,  p-value: 1.92e-10
### ADF Test                                                               ###
library("tseries")
adf.test(x)
 Augmented Dickey-Fuller Test
data:  x
Dickey-Fuller = -2.6073, Lag order = 2, p-value = 0.341
alternative hypothesis: stationary

### Forecast, package forecast                                             ###
library(forecast)
model <- rw_model(x)
forecast(model, h = 10) |> autoplot()
fit <- ets(x)
plot(forecast(fit, h = 10))
### AR(1) model    
t<- rep(1,24)
eq_ar1<- lm(x[2:25] ~ x[1:24] +t)
summary(eq_ar1)
detach(aktien)

### Zeitraum von bis 6 MRZ 2026                                            ###
aktien2<- read.csv("aktien2.csv", header=TRUE)
str(aktien2)
attach(aktien2)
str(cyber)
x<- cyber
plot(x, main= "Time series",typ="l")
t<- 1:39
kurs<- lm(x ~ t)
summary(kurs)
Call: lm(formula = x ~ t)
Residuals:
   Min     1Q Median     3Q    Max 
-43.13 -12.85  -1.95  11.45  57.32 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 218.0283     6.9326   31.45  < 2e-16 ***
t             3.9217     0.3021   12.98 2.39e-15 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 21.23 on 37 degrees of freedom
Multiple R-squared:   0.82,     Adjusted R-squared:  0.8151 
F-statistic: 168.5 on 1 and 37 DF,  p-value: 2.393e-15
x_fit<- fitted(kurs)
lines(x_fit,col = "red")
library(sandwich)
estfun(kurs)
meat(kurs)
meatHC(kurs)
bread(kurs)
sandwich(kurs)
vcovHC(kurs, type = "HC")
res<- resid(kurs)
acf(res)
res_sq<- res^2
acf(res_sq)
res<- ts(res, freq = 5, start =1)
plot(res, main ="Time series", ylab ="Residuals")

library("tseries")
adf.test(x)
    Augmented Dickey-Fuller Test
data:  x
Dickey-Fuller = -1.597, Lag order = 3, p-value = 0.7312
alternative hypothesis: stationary 
var(x)
sd(x)
x1<- x[1:25]
t1<- 1:25
eq1<- lm(x1 ~ t1)
summary(eq1)
x2<- x[26:39]
t2<- 1:14
eq2<- lm(x2 ~ t2)
summary(eq2)
anova(eq1,eq2)
sis of Variance Table

Response: x1
          Df Sum Sq Mean Sq F value   Pr(>F)    
t1         1  17239 17239.0  115.56 1.92e-10 ***
Residuals 23   3431   149.2                     
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"x2"’ removed because response differs from 
model 1
anova(kurs, eq1)
Analysis of Variance Table
Response: x
          Df Sum Sq Mean Sq F value    Pr(>F)    
t          1  75974   75974  168.53 2.393e-15 ***
Residuals 37  16679     451                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"x1"’ removed because response differs 
  from model 1

sd(x1)
sd(x2)
var(x1)
var(x2)
acf(x)
t<- 1:38
### AR(1) Model                                                            ###
eq_ar1<- lm(x[2:39] ~ x[1:38]+t)
summary(eq_ar1)
res1<- resid(eq_ar1)
res1<- ts(res1, freq =5, start = 2)
plot(res1, main ="Time series", ylab ="residuals")
dy<- x[2:39] - x[1:38]
dy<- ts(dy, freq = 5, start =1)
plot(dy,main ="Time series", ylab ="difference series")
eq_rw<- lm(dy ~ 1)
summary(eq_rw)
eq_rw_no<- lm(dy ~ -1)
summary(eq_rw_no)
### Forecast, package forecast                                             ###
library(forecast)
model <- rw_model(x)
forecast(model, h = 10) |> autoplot()
fit <- ets(x,model="AAN")
plot(forecast(fit, h = 10))
detach(aktien2)
### Course of sbux                                                         ###
y<- sbux
plot(y, type="l")
kurs_s<- lm(y~t)
summary(kurs_s)
### Estimation of trend model                                              ###
Call: lm(formula = y ~ t)
Residuals:
    Min      1Q  Median      3Q     Max 
-3.9755 -1.0969  0.4619  1.9122  3.7585 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 90.79400    0.94680  95.896  < 2e-16 ***
t            0.27886    0.06369   4.379 0.000219 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 2.296 on 23 degrees of freedom
Multiple R-squared:  0.4546,    Adjusted R-squared:  0.4309 
F-statistic: 19.17 on 1 and 23 DF,  p-value: 0.0002192
### ADF Test                                                               ###
library(tseries)
adf.test(y, alternative = c("stationary", "explosive"),
 k = trunc((length(x)-1)^(1/3)))


### A relation between x and y ?                                           ###
xy<- lm(x ~ y)
summary(xy)
yx<- lm(y ~ x)
summary(yx)
plot(x,y)

aktien3<- read.csv("aktien3.csv", header = TRUE)
str(aktien3)
attach(aktien3)
y<- ts(sbux, freq =5, start =1)
plot(y, main = "AR(1) Model", ylab = "y")
t<- 1:44
eq_sb<- lm(y ~t)
fit<- fitted(eq_sb)
lines(fit, col="red", add =TRUE)
acf(y)
summary(eq_sb)
Call:lm(formula = y ~ t)
Residuals:
    Min      1Q  Median      3Q     Max 
-3.6302 -1.3784  0.1151  1.0115  3.9157 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 91.62074    0.60268  152.02  < 2e-16 ***
t            0.18941    0.02333    8.12 3.81e-10 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.965 on 42 degrees of freedom
Multiple R-squared:  0.6109,    Adjusted R-squared:  0.6016 
F-statistic: 65.93 on 1 and 42 DF,  p-value: 3.809e-10
library(tseries)
adf.test(y)
 Augmented Dickey-Fuller Test
data:  y
Dickey-Fuller = -4.1457, Lag order = 3, p-value = 0.01277
alternative hypothesis: stationary
res<- resid(eq_sb)
res<- ts(res, freq = 5, start=1)
plot(res, main = "Time series", ylab = "Residuals")
y1<- c(NA,y)
y2<- c(NA,y1)
eq_ar2<- lm(y ~ y1[1:44]+ y2[1:44])
summary(eq_ar2)
res1<- resid(eq_ar2)
res1<- ts(res1, freq=5, start=1)
plot(res1, main ="AR(2) model", ylab="Residuals")
fit2<- fitted(eq_ar2)
fit2<- ts(fit2, freq = 5, start =1)
lines(fit2, col ="red", type="l")
legend("bottomright", paste("The red line is the fitted value."))
### AR2 Model Call lm(formula = y ~ y1[1:44] + y2[1:44])
Residuals:
    Min      1Q  Median      3Q     Max 
-2.7344 -0.9838 -0.1393  0.9477  3.2991 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  18.9308     7.1180   2.660   0.0113 *  
y1[1:44]      0.9295     0.1582   5.876 7.69e-07 ***
y2[1:44]     -0.1245     0.1498  -0.832   0.4107    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.368 on 39 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared:  0.7591,    Adjusted R-squared:  0.7468 
F-statistic: 61.46 on 2 and 39 DF,  p-value: 8.793e
eq_ar1<- lm(y ~ y1[1:44])
summary(eq_ar1)
res2<- resid(eq_ar1)
res2<- ts(res2, freq =5, start=1)
plot(res2,main="AR(1) model", ylab="Residuals")

### AR1 Model Call:lm(formula = y ~ y1[1:44])
Residuals:
    Min      1Q  Median      3Q     Max 
-3.0649 -0.8333 -0.1127  1.0330  3.2099 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 16.35833    6.44278   2.539    0.015 *  
y1[1:44]     0.83192    0.06721  12.377 1.97e-15 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.354 on 41 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.7889,    Adjusted R-squared:  0.7837 
F-statistic: 153.2 on 1 and 41 DF,  p-value: 1.971e-15
fit<- fitted(eq_ar1)
fit<- ts(fit, freq = 5, start=1)
lines(fit, col = "red", type="l")
legend("bottomright", "The red line is the fitted value.")
library(forecast)
plot(forecast(fit, h= 10))

x<- cyber
x<- ts(x, freq= 5, start =1)
eq<- lm(x~t)
summary(eq)
res_x<- resid(eq)
fit_x<- fitted(eq)
plot(x, main= "Time series", ylab="x")
lines(fit_x, col="red")
library(strucchange)

eq_cusum<- efp(x~t,type = "Rec-CUSUM")
plot(eq_cusum)
plot(eq_cusum, alpha = 0.01, alt.boundary = TRUE)
## calculate corresponding test statistic
sctest(eq_cusum)
        Recursive CUSUM test

data:  eq_cusum
S = 0.60109, p-value = 0.4113

aktien4<- read.csv("aktien4.csv", header=TRUE)
str(aktien4)
attach(aktien4)
x<- cyber
x<- ts(x, freq=5, start =1)
plot(x, main ="Actual value", ylab = "x")
acf(x)
t<- 1:55
eq<- lm(x~t)
summary(eq)
library(tseries)
adf.test(x)
library(strucchange)
eq_cusum<- efp(x~t,type = "Rec-CUSUM")
plot(eq_cusum)
sctest(eq_cusum)
 Recursive CUSUM test
data:  eq_cusum
S = 1.3893, p-value = 0.0008451

x1<- c(NA,x)
x2<- c(NA,x1)
eq_ar2<- lm(x ~ x1[1:55]+x2[1:55])
summary(eq_ar2)

eq_ar1<- lm(x ~ x1[1:55])
summary(eq_ar1)
res<- resid(eq_ar1)
res<- ts(res, freq = 5, start=1)
plot(res, type="l")

library(strucchange)
eq_cusum2<- efp(x~x1[1:55],type = "Rec-CUSUM")
plot(eq_cusum2)
sctest(eq_cusum2)
Recursive CUSUM test
data:  eq_cusum2
S = 0.61263, p-value = 0.3896

plot(x, main ="Actual value", ylab = "x")
fit<- fitted(eq_ar1)
fit<- ts(fit, freq=5, start=1)
lines(fit, col ="red", type="l")

x_diff<- x[2:55] - x[1:54]
library(tseries)
adf.test(x_diff)
Augmented Dickey-Fuller Test
data:  x_diff
Dickey-Fuller = -3.4042, Lag order = 3, p-value = 0.06455
alternative hypothesis: stationary

eq_rw<- lm(x_diff ~ 1)
summary(eq_rw)
fit<- fitted(eq_rw)
fit<- ts(fit, freq=5, start=1)
plot(fit, col="red", type ="l")