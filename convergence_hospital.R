https://de.wikipedia.org/wiki/Regel_von_de_L%E2%80%99Hospital
par(mfrow=c(3,1))
f<- function(x){
result<- cos(x) -1
return(result)
}
curve(f, -3,3)

g<- function(x){
result<- tan(x)
return(result)
}
curve(g, -3, 3)

### Hospital OK -> convergence OK  Lim = 0
f_h<- function(x)
result<- (cos(x)-1)/tan(x)
return(result)
}
curve(f_h, -3, 3)

par(mfrow=c(3,1))
f<- function(x){
result<- sqrt(x)
return(result)
}
curve(f, 0,3)
g<- function(x){
result<- log(x)
return(result)
}
curve(g, 0,3)
### Hospital OK -> convergence OK   Lim = INF
f_h<- function(x){
result<- sqrt(x)/log(x)
return(result)
}
curve(f_h, 0,3)


par(mfrow=c(3,1))
f<- function(x){
result<- sin(x) + 2*x
return(result)
}
curve(f, -3,3)
g<- function(x){
result<- cos(x) +2*x
return(result)
}
curve(g, -3,3)
### Hospital OK -> convergence OK   Lim = 1
f_h<- function(x){
result<- (sin(x)+2*x)/(cos(x)+2*x)
return(result)
}
curve(f_h, -3,3)


par(mfrow=c(3,1))
f<- function(x){
result<- sin(x) - x
return(result)
}
curve(f, -3,3)
g<- function(x){
result<- x*(1 -cos(x))
return(result)
}
curve(g, -3,3)
### Hospital OK -> convergence OK   Lim = 1/3 Landau-Kalkül : taylor series
f_h<- function(x){
result<- (sin(x)-x)/(x*(1-cos(x)))
return(result)
}
curve(f_h, -3,3)