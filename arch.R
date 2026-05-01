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
summary(x.arch)                    
Call:
garch(x = x, order = c(0, 5))
Model:
GARCH(0,5)
Residuals:
     Min       1Q   Median       3Q      Max 
-6.03185 -0.49415  0.07484  0.65290  5.65734 
Coefficient(s):
    Estimate  Std. Error  t value Pr(>|t|)    
a0 4.061e-05   1.712e-06   23.717  < 2e-16 ***
a1 8.193e-02   1.796e-02    4.562 5.07e-06 ***
a2 1.288e-01   1.471e-02    8.757  < 2e-16 ***
a3 5.283e-02   1.967e-02    2.686  0.00723 ** 
a4 1.287e-01   1.963e-02    6.556 5.54e-11 ***
a5 1.037e-01   1.983e-02    5.232 1.68e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Diagnostic Tests:
        Jarque Bera Test
data:  Residuals
X-squared = 680.5, df = 2, p-value < 2.2e-16
        Box-Ljung test
data:  Squared.Residuals
X-squared = 6.4882e-05, df = 1, p-value = 0.9936

plot(x.arch)                        
x.garch <- garch(x, order = c(2,2))  # Fit GARCH(2,2) 
### Compare with Panel 6 (p.633)                                           ###
summary(x.garch)                     # Diagnostic tests
Call:
garch(x = x, order = c(2, 2))
Model:
GARCH(2,2)

Residuals:
     Min       1Q   Median       3Q      Max 
-6.53673 -0.50220  0.07656  0.67786  5.06804 

Coefficient(s):
    Estimate  Std. Error  t value Pr(>|t|)
a0 1.489e-06          NA       NA       NA
a1 3.086e-02          NA       NA       NA
a2 6.427e-02          NA       NA       NA
b1 1.138e-01          NA       NA       NA
b2 7.735e-01          NA       NA       NA
Diagnostic Tests:
        Jarque Bera Test
data:  Residuals
X-squared = 780.14, df = 2, p-value < 2.2e-16
        Box-Ljung test
data:  Squared.Residuals
X-squared = 2.8445, df = 1, p-value = 0.09169
plot(x.garch)  
res22<- resid(x.garch)                      
plot(res22)
