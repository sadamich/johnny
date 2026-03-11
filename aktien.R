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