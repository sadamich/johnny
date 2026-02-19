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
unitrootTest(y, lags = 4, type = "nc", title = NULL, 
    description = NULL)    








