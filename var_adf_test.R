### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press, p.676                 ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 27 Interest and bond rates p.674 ###
###Usage: ###
 adf.test(x, alternative = c("stationary", "explosive"),
 k = trunc((length(x)-1)^(1/3)))
library("tseries")
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
x<- AAA
 adf.test(x, alternative = c("stationary", "explosive"),
 k = trunc((length(x)-1)^(1/3)))
 Augmented Dickey-Fuller Test
data:  x (AAA) 
Dickey-Fuller = -1.4946, Lag order = 8, p-value = 0.7922
H0 (stochastic trend) was not rejectet.
 
x<- DAAA
adf.test(x, alternative = c("stationary", "explosive"),
 k = trunc((length(x)-1)^(1/3)))
Augmented Dickey-Fuller Test
data:  x (DAAA)
Dickey-Fuller = -6.9638, Lag order = 8, p-value = 0.01
alternative hypothesis: stationary
H0(stochastic trend) was rejected.

x<- US3MTBIL
adf.test(x, alternative = c("stationary", "explosive"),
 k = trunc((length(x)-1)^(1/3)))
 Augmented Dickey-Fuller Test
data:  x(US3MTBIL)
Dickey-Fuller = -2.1834, Lag order = 8, p-value = 0.5007
alternative hypothesis: stationary
H_0 (stochastic trend) was not reject

x<- DUS3MT
adf.test(x, alternative = c("stationary", "explosive"),
 k = trunc((length(x)-1)^(1/3)))
Augmented Dickey-Fuller Test
data:  x
Dickey-Fuller = -8.087, Lag order = 8, p-value = 0.01
alternative hypothesis: stationary
H0 (stochastic trend) was reject.

