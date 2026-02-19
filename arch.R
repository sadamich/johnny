### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
library(forecast)
xm702<- read.csv("xm702.csv", header =TRUE)
str(xm702)
attach(xm702)
### Autocorrelation functions, exhibit 7 25 a (p.632(                      ###
Acf(DJRET,lag.max = NULL,plot = TRUE,na.action = na.contiguous,
  demean = TRUE)
Pacf(DJRET,lag.max = NULL,plot = TRUE, na.action = na.contiguous,
  demean = TRUE)
Acf(DJRET^2,lag.max = NULL,plot = TRUE,na.action = na.contiguous,
  demean = TRUE)
Pacf(DJRET^2,lag.max = NULL,plot = TRUE, na.action = na.contiguous,
  demean = TRUE)
### Exhibit 7 25 b (p.632)                                                 ###
hist(DJRET)
summary(DJRET)
library(tseries)
x<- na.remove(DJRET)
x.arch <- garch(x, order = c(0,5))  # Fit ARCH(5) 
### Compare with Panel 3 (p.632)                                           ###
summary(x.arch)                     # Diagnostic tests
plot(x.arch)                        
x.garch <- garch(x, order = c(2,2))  # Fit GARCH(2,2) 
### Compare with Panel 6 (p.633)                                           ###
summary(x.garch)                     # Diagnostic tests
plot(x.garch)                        


