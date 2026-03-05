### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 17 (p.717)                                                  ###
xr717<- read.csv("xr717.csv",header =TRUE)
str(xr717)
attach(xr717)
y<- ts(TOYOTA, freq =12, start = 1980)
plot(y, main = "Time series", ylab = "Toyota")
library(forecast)
Acf(y,lag.max = NULL,type = "correlation", plot = TRUE,na.action = na.contiguous,
  demean = TRUE)
Pacf(y,lag.max = NULL,plot = TRUE,na.action = na.contiguous,demean = TRUE)

### Smoothing 
fit <- ets(y)
plot(forecast(fit, h = 10))

fit <- HoltWinters(y, gamma = FALSE)
plot(forecast(fit))

### ADF Test                                                                  ###
library(tseries)
y<- na.remove(y)
adf.test(y)
 Augmented Dickey-Fuller Test
data:  y
Dickey-Fuller = -2.0713, Lag order = 6, p-value = 0.5459
alternative hypothesis: stationary

library(fUnitRoots)
unitrootTest(y, lags = 4, type = "ct", title = NULL, 
    description = NULL)    
Title:
 Augmented Dickey-Fuller Test

Test Results:
  PARAMETER:
    Lag Order: 4
  STATISTIC:
    DF: -2.7009
  P VALUE:
    t: 0.2373 

### Problem e                                                             ###
s1<- c(0,1,0,0,0,0, 0,0,0,0,0,0)
d1<- rep(s1, 20)
s2<- c(0,0,1,0,0,0, 0,0,0,0,0,0)
d2<- rep(s2, 20)
s3<- c(0,0,0,1,0,0, 0,0,0,0,0,0)
d3<- rep(s3,20)
s4<- c(0,0,0,0,1,0, 0,0,0,0,0,0)
d4<- rep(s4,20)
s5<- c(0,0,0,0,0,1, 0,0,0,0,0,0)
d5<- rep(s5,20)
s6<- c(0,0,0,0,0,0, 1,0,0,0,0,0)
d6<- rep(s6,20)
s7<- c(0,0,0,0,0,0, 0,1,0,0,0,0)
d7<- rep(s7,20)
s8<- c(0,0,0,0,0,0, 0,0,1,0,0,0)
d8<- rep(s8,20)
s9<- c(0,0,0,0,0,0, 0,0,0,1,0,0)
d9<- rep(s9,20)
s10<-c(0,0,0,0,0,0, 0,0,0,0,1,0)
d10<- rep(s10,20)
s11<-c(0,0,0,0,0,0, 0,0,0,0,0,1)
d11<- rep(s11,20)
aufgabe_e<- lm(y ~ d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11)
summary(aufgabe_e)

restoy<- resid(aufgabe_e)
restoy<-  ts(restoy, freq =12, start = 1980)
plot(restoy, main = "Time series", ylab ="residuals")

library(tseries)
adf.test(restoy)
 Augmented Dickey-Fuller Test

data:  restoy
Dickey-Fuller = -1.7406, Lag order = 6, p-value = 0.6849
alternative hypothesis: stationary

restoy_1<- c(NA, restoy)
restoy_d<- restoy - restoy_1[1:240]
restoy_d<- na.remove(restoy_d)
adf.test(restoy_d)
 Augmented Dickey-Fuller Test
data:  restoy_d
Dickey-Fuller = -9.1626, Lag order = 6, p-value = 0.01
alternative hypothesis: stationary
Warning message:
In adf.test(restoy_d) : p-value smaller than printed p-value

### Restoy is I(1)                                                         ###






