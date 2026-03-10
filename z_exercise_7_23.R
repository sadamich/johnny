### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 23 (p.720)                                                  ###
xr723 <- read.csv("xr723.csv", header =TRUE)
str(xr723)
attach(xr723)
### Problem (a) Graphical interpretation                                   ###
g<- ts(P_G, freq = 12, start = 1957)
plot(g, main ="Time series", ylab="Price in German")
uk<- ts(P_UK, freq = 12, start = 1957)
plot(uk, main = "Time series", ylab = "Price in UK")
x_g<- ts(X_G, freq= 12, start = 1957)
plot(x_g, main ="Time series",ylab = "Exchange course")
x_uk<- ts(X_UK, freq= 12, start = 1957)
plot(x_uk, main ="Time series",ylab = "Exchange course")
### Problem (b) ADF Test and Johansen Test                                 ###
library(tseries)
adf.test(g)
Augmented Dickey-Fuller Test
data:  g
Dickey-Fuller = -2.6412, Lag order = 7, p-value = 0.3069
alternative hypothesis: stationary
uk<- na.omit(uk)
adf.test(uk)
 Augmented Dickey-Fuller Test
data:  uk
Dickey-Fuller = -2.4288, Lag order = 7, p-value = 0.3968
alternative hypothesis: stationary
adf.test(x_g)
Augmented Dickey-Fuller Test
data:  x_g
Dickey-Fuller = -1.8984, Lag order = 7, p-value = 0.6212
alternative hypothesis: stationary
adf.test(x_uk)       
        Augmented Dickey-Fuller Test
data:  x_uk
Dickey-Fuller = -2.6859, Lag order = 7, p-value = 0.288
alternative hypothesis: stationary

### Problem (c) ADF Test for the relation between German and UK            ###
x_g_uk<- log(X_G/X_UK)
logp_g<- log(P_G)
logp_uk<- log(P_UK)
adf.test(x_g_uk)
 Augmented Dickey-Fuller Test
data:  x_g_uk
Dickey-Fuller = -1.5033, Lag order = 7, p-value = 0.7885
alternative hypothesis: stationary
adf.test(logp_g)
   Augmented Dickey-Fuller Test
data:  logp_g
Dickey-Fuller = -0.7736, Lag order = 7, p-value = 0.9639
alternative hypothesis: stationary
logp_uk<- na.omit(logp_uk)
adf.test(logp_uk)  
 Augmented Dickey-Fuller Test
data:  logp_uk
Dickey-Fuller = -1.3178, Lag order = 7, p-value = 0.867
alternative hypothesis: stationary
### PPP Theory                                                             ###
ppp<- x_g_uk - logp_g + logp_uk
adf.test(ppp)   
Augmented Dickey-Fuller Test
data:  ppp
Dickey-Fuller = -2.0429, Lag order = 7, p-value = 0.5601
alternative hypothesis: stationary
### PPP is not stationary                                                  ###
          