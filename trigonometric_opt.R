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



f_m<- function(x){
result<- -((1-cos(x)-(pi/2 -x)*sin(x)))^2
return(result)
}
m<- maxLik(f_m, start= 0)
summary(m)
curve(f_m, -5,5, xlab="x",ylab="f(x)_m")


