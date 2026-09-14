

### base = 2
f_2<- function(x){
result<- 2^x
return(result)
}
curve(f_2, -5,5)

f_2_e<- function(x){
result<- exp(x*log(2))
return(result)
}
curve(f_2_e, -5, 5, col="red")


### To the exponential function : Approximation
for (i in 1:10){
x<- -5:5
f<- function(x)
(1+x/i)^i
}
curve(f, -5,5)
f(1)
[1] 2.593742
exp(1)
[1] 2.718282
curve(exp(x), -5,5, add =TRUE, col="red")
for (i in 1:1000){
x<- -5:5
f2<- function(x)
(1+x/i)^i
}
curve(f2, -5,5)
f2(1)
[1] 2.716924
for (i in 1:10000){
x<- -5:5
f3<- function(x)
(1+x/i)^i
}
curve(f3, -5,5)
f3(1)
[1] 2.718146