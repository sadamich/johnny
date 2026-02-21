### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 21 (p.718)                                                  ###
xr721 <- read.csv("xr721.csv", header =TRUE)
str(xr721)
attach(xr721)
### a linear regression                                                    ###
eq1<- lm(GC~ PG+RI)
summary(eq1)
### b ADF Test                                                             ###
library(tseries)
adf.test(GC)
 Augmented Dickey-Fuller Test
data:  GC
Dickey-Fuller = -2.6668, Lag order = 3, p-value = 0.3165
alternative hypothesis: stationary
adf.test(PG)
 Augmented Dickey-Fuller Test
data:  PG
Dickey-Fuller = -2.4539, Lag order = 3, p-value = 0.3983
alternative hypothesis: stationary
adf.test(RI)
 Augmented Dickey-Fuller Test
data:  RI
Dickey-Fuller = -2.2651, Lag order = 3, p-value = 0.4708
alternative hypothesis: stationary

