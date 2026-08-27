### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Seite 210 - 211 

f<- function(x){
result<- ((1-cos(x)-(pi/2 -x)*sin(x)))^2
return(result)
}
optimize(f, c(0.1,pi/2))

curve(f(x), -5,5, xlab= "x", ylab="f(x)")

### Use of maxLik
library(maxLik)
f_m<- function(x){
result<- -((1-cos(x)-(pi/2 -x)*sin(x)))^2
return(result)
}
m<- maxLik(f_m, start= 0)
summary(m)
curve(f_m, -5,5, xlab="x",ylab="f(x)_m")


f_m2<- function(x){
result<- cos(2*x)+sin(x) -1
return(result)
}
m2<- maxLik(f_m2, start= pi)
summary(m2)
curve(f_m2, -5,5, xlab="x",ylab="f(x)_m")
f_m2(0)
[1] 0
f_m2(pi)
[1] 0:  2.220446e-16
f_m2(pi/6)
[1] 0
f_m2(5/6*pi)
[1] 0


f_m3<- function(x){
result<- sin(2*x)+cos(x)
return(result)
}
m3<- maxLik(f_m2, start= pi)
summary(m3)
curve(f_m3, -5,5, xlab="x",ylab="f(x)_m")

f_m4<- function(x){
result<- sin(x)- sqrt(3)*cos(x)
return(result)
}
m4<- maxLik(f_m2, start= pi)
summary(m4)
curve(f_m4, -5,5, xlab="x",ylab="f(x)_m")
f_m4(5/6*pi)
[1] 2      (Max value)
f_m4(11/6*pi)
[1] -2     (Min value)
f_m4(4/3*pi)
[1] 0 : 9.992007e-16
f_m4(3/2*pi)
[1] -1
f_m4(0)
[1] -1.732051
f_m4(pi/6)
[1] -1
f_m4(pi/4)
[1] -0.5176381
f_m4(pi/3)
[1] 0 : -2.220446e-16