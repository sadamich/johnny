### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 18 (p.717)                                                  ###
xr718 <- read.csv("xr718.csv", header =TRUE)
str(xr718)
attach(xr718)
### a   Graphical interpretation                                           ###
y<- NUCEP
y<- ts(y, freq = 12, start = 1973)
plot(y, main ="Time series", ylab = "Nuclear electric")
str(NUCEP)
OBS
y_1<- NUCEP[205:324]
y_1<- ts(y_1, freq = 12, start = 1990)
plot(y_1, main = "Time series", ylab = "Nuclear electric")

### b Estimate by a linear regression                                      ###
t <- 1:120
nuc<- lm(log(y_1) ~ t)
summary(nuc)
Call: lm(formula = log(y_1) ~ t)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.23716 -0.06591  0.00211  0.07935  0.20147 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 6.2506226  0.0180065 347.132  < 2e-16 ***
t           0.0014947  0.0002604   5.739 7.62e-08 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.0976 on 117 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.2197,    Adjusted R-squared:  0.213 
F-statistic: 32.94 on 1 and 117 DF,  p-value: 7.622e-08
### t is significant                                                       ###
com<- decompose(y_1)
plot(com)
### ADF Test I(2)?                                                         ###
library(tseries)
y_1<- na.omit(y_1)
adf.test(y_1)
 Augmented Dickey-Fuller Test
data:  y_1
Dickey-Fuller = -2.6644, Lag order = 4, p-value = 0.3006
alternative hypothesis: stationary
dy<- NUCEP[205:324]- NUCEP[204:323]
dy<- na.omit(dy)
adf.test(dy)
 Augmented Dickey-Fuller Test
data:  dy
Dickey-Fuller = -8.3349, Lag order = 4, p-value = 0.01
alternative hypothesis: stationary

rep((1,0,0,0,0,0,0,0,0,0,0,0), 10)
### I(2) is rejected. Seasonal effect is doubtful.                          ###
### c ARCH                                                                  ###

dum2<- c(0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0)
dum3<- c(0, 0, 1, 0, 0,   0, 0, 0, 0, 0, 0, 0)
dum3<- rep(dum3, 10)
dum4<- c(0, 0, 0, 1, 0,   0, 0, 0, 0, 0, 0, 0)
dum4<- rep(dum4, 10)
dum5<- c(0, 0, 0, 0, 1,   0, 0, 0, 0, 0, 0, 0)
dum5<- rep(dum5, 10)
dum6<- c(0, 0, 0, 0, 0,   1, 0, 0, 0, 0, 0, 0)
dum6<- rep(dum6, 10)
dum7<- c(0, 0, 0, 0, 0,   0, 1, 0, 0, 0, 0, 0)
dum7<- rep(dum7, 10)
dum8<- c(0, 0, 0, 0, 0,   0, 0, 1, 0, 0, 0, 0)
dum8<- rep(dum8, 10)
dum9<- c(0, 0, 0, 0, 0,   0, 0, 0, 1, 0, 0, 0)
dum9<- rep(dum9, 10)
dum10<- c(0, 0, 0, 0, 0,  0, 0, 0, 0, 1, 0, 0)
dum10<- rep(dum10, 10)
dum11<- c(0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 1, 0)
dum11<- rep(dum11,10)
dum12<- c(0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 1)
dum12<- rep(dum12, 10)
### b linear regression with dummies  
y_1<- ts(y_1, freq = 12, start = 1990)                                    ###
eq1 <- lm(log(y_1) ~ t+ dum2+ dum3 + dum4 + dum5 + dum6 + dum7
               + dum8+ dum9 + dum10 + dum11 + dum12)
summary(eq1)

Call:
lm(formula = log(y_1) ~ t + dum2 + dum3 + dum4 + dum5 + dum6 + 
    dum7 + dum8 + dum9 + dum10 + dum11 + dum12)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.105710 -0.031849 -0.003653  0.034771  0.093629 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.3629377  0.0174269 365.121  < 2e-16 ***
t            0.0014987  0.0001338  11.199  < 2e-16 ***
dum2        -0.1333842  0.0223396  -5.971 3.19e-08 ***
dum3        -0.1596268  0.0223408  -7.145 1.19e-10 ***
dum4        -0.2818302  0.0223428 -12.614  < 2e-16 ***
dum5        -0.1792139  0.0223456  -8.020 1.50e-12 ***
dum6        -0.1004425  0.0223492  -4.494 1.79e-05 ***
dum7        -0.0045528  0.0223537  -0.204   0.8390    
dum8         0.0063521  0.0223589   0.284   0.7769    
dum9        -0.1083097  0.0223649  -4.843 4.38e-06 ***
dum10       -0.1853371  0.0223717  -8.284 3.92e-13 ***
dum11       -0.1506828  0.0223793  -6.733 8.82e-10 ***
dum12       -0.0470906  0.0229611  -2.051   0.0427 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.04995 on 106 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.8148,    Adjusted R-squared:  0.7938 
F-statistic: 38.86 on 12 and 106 DF,  p-value: < 2.2e-16

res<- resid(eq1)
res<- ts(res, freq = 12, start = 1990)
plot(res, main =" 1990 - 1998", ylab = "Residuals")
plot(res)
acf(res)
hist(res)

### c  ARMA Model for the residuals of the model b                          ###
library(tseries)
res_arma<- arma(res, order = c(1,1))
summary(res_arma)
res_arma2<- arma(res, order = c(2,2))
summary(res_arma2)
res_arma3<- arma(res, order = c(2,0))
summary(res_arma3)