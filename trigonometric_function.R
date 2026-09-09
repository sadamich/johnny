### Trigonometric function ###
sin(pi/6)
cos(pi/6)
tan(pi/6)

sin(pi/4)
cos(pi/4)
tan(pi/4)

sin(pi/3)
cos(pi/3)
tan(pi/3)

sin(pi/6)^2+cos(pi/6)^2
sin(pi/4)^2+cos(pi/4)^2
sin(pi/3)^2+cos(pi/3)^2

curve(cos(x), -10,10, lty =2 , ylim=c(-1.5,1.5), 
           xlab= "x", yaxs ="i", ylab = "cos(x)")

### The half angle formel
sin(pi/8)
[1] 0.3826834
sin(3/8*pi)
[1] 0.9238795
cos(3/8*pi)
[1] 0.3826834     ### sin(pi/8) = cos(3/8*pi)

### The inverse of sin/cos/tan
tan(pi/3)
[1] 1.732051
atan(1.732051)
[1] 1.047198
pi/3
[1] 1.047198
cos(pi/3)
[1] 0.5
acos(0.5)
[1] 1.047198
sin(pi/3)
[1] 0.8660254
asin(0.8660254)
[1] 1.047198

atan2(y, x)

cospi(pi/3)
[1] -0.9890273
sinpi(pi/6)
tanpi(x)
### Euler
exp(*pi) + 1
f_euler<- function(x){
result <- exp(pi*x)+1
return(result)
}
m<- maxLik(f_euler, start= pi)????
summary(m)
curve(f_euler, -5,5, xlab="x",ylab="f(x)_m")