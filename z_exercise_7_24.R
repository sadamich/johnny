### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 24 (p.720)                                                  ###
xr724 <- read.csv("xr724.csv", header =TRUE)
str(xr724)
attach(xr724)
### a Graphical interpretation and ADF Test                                ###
sp<- ts(SP, freq = 1, start = 1871)
plot(sp, main = "Time series", ylab = "SP")
div<- ts(DIV, freq =1, start = 1871)
plot(div, main = "Time series", ylab ="DIV")
library(tseries)
adf.test(sp)
   Augmented Dickey-Fuller Test
data:  sp
Dickey-Fuller = 2.1625, Lag order = 4, p-value = 0.99
alternative hypothesis: stationary
adf.test(div)
   Augmented Dickey-Fuller Test
data:  div
Dickey-Fuller = 3.3353, Lag order = 4, p-value = 0.99
alternative hypothesis: stationary
> 