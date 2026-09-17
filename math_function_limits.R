

f1<- function(x){
result<- (x^2-1)/(x-1)
return(result)
}
curve(f1, -3,3)

f2<- function(x){
result<- 1/x*(1/2 - 1/(2+x))
return(result)
}
curve(f2, -3,3)
f2(0.1)
[1] 0.2380952
f2(0.01)
[1] 0.2487562
f2(0.0001)
[1] 0.2499875  : lim (f2) = 1/4 = 0.25

f3<- function(x){
result<- (sqrt(x+4)-2)/x
return(result)
}
curve(f3, -3,3)
f3(0.1)
[1] 0.2484567
f3(0.0001)
[1] 0.2499984 : lim (f3) = 1/4 = 0.25

f4<- function(x){
result<- (x^2 -9)/(x+3)
return(result)
}
curve(f4, -5, 0)
f4(-2.9)
[1] -5.9
f4(-2.99999)
[1] -5.99999  : lim (f4) = -6

f5<- function(x){
result<- (x^3 -1)/(x^2-3*x+2)
return(result)
}
curve(f5, 0 ,3)
### f5 == f6 : Non linear function
f6<- function(x){
result<- (x^2+x+1)/(x-2)
return(result)
}
curve(f6, 0 ,3, ylim= c(-10, 10))
f6(1)
f6(0.9)
[1] -2.463636
f6(1.1)

### Non linear function
f7<- function(x){
result<- (sqrt(x+3)-2)/(x-1)
return(result)
}
curve(f7, -3,3)
f7(1)
[1] NaN : not definit
f7(0.9)
[1] 0.2515823
f7(0.9999)
[1] 0.2500016
f7(1.001)
[1] 0.2499844   : lim (f7) = 1/4 = 0.25

### Non linear function 
f8<- function(x){
result<- (x - 4)/(sqrt(x)-2)
return(result)
}
curve(f8, 0, 5)
f8(3.9)
[1] 3.974842
f8(3.9999)
[1] 3.999975   : lim (f8) = 4