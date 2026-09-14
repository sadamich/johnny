### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 29  Primary metal industries ###
xm729<- read.csv("xm729.csv", header = TRUE)
attach(xm729)
str(xm729)
### n * 1 = 37 (years) * 1  
str(OBS)
int [1:37] 1958 1959 1960    : 1958 - 1994 : 37 years
str(LOGPROD_1)
 num [1:37] 5.38 5.49 5.48 5.42 5.42 ...
str(LOGLAB_1)
num [1:37] 4.46 4.54 4.58 4.53 4.54 .
str(LOGCAP_1)
num [1:37] 11 11 11 11 11 .
summary(eq)
### 3 * 3
d<- diag(3)
d
###  (1* 3) *(3 * 3) = 1 * 3 
a<- c(1,1,1)
t(a)%*%d

### 26 * 26
d<- diag(26)
d
###  (1* 26) *(26* 26) = 1 * 26
a<- rep(1,26)
t(a)%*%d

z<- d%*%a
str(z)

v<- rep(1,37)
w<- rbind(rep(v, 26))
str(w)

D<- cbind(rep(z, 37))
str(D)

Y<- rbind(LOGPROD_1, LOGPROD_2,LOGPROD_3)
X1<- rbind(LOGLAB_1, LOGLAB_2,LOGLAB_3)
X2<- rbind(LOGCAP_1, LOGCAP_2,LOGCAP_3)
Y
d11<- rep(1,37)
d12<- rep(0,37)
d13<- rep(0,37)
d1<- rbind(d11,d12,d13)
str(d1)
d21<- rep(0,37)
d22<- rep(1,37)
d23<- rep(0,37)
d2<- rbind(d21,d22,d23)
str(d2)
d31<- rep(0,37)
d32<- rep(0,37)
d33<- rep(1,37)
d3<- rbind(d31,d32,d33)
str(d3)

eq<- lm(Y~X1+X2+d1+d2+d3)
summary(eq)