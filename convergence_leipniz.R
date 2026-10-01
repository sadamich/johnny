https://de.wikipedia.org/wiki/Leibniz-Kriterium
n<- 1:100
f<- sum( (-1)^(n+1) / n)
f
[1] 0.6931472

n<- 1:10000
f<- sum( (-1)^(n+1) / n)
f
[1] 0.6930972
log(2)

[1] 0.6931472

n<- 0:100000
f<- sum( ((-1)^(n)) /(2*n+1))
f
[1] 0.7854007
atan(1)
[1] 0.7853982
pi/4
[1] 0.7853982

### Exponent a^0 = 1  
f1<- function(n){
result<- 10^(1/n)
return(result)
}
curve(f1, -3,3)

### 
f2<- function(n){
result<- 10^(n)/factorial(n)
return(result)
}
curve(f2, -5,5)
### Geometiric sequence
f3<- function(n){
result<- 10^n
return(result)
}
curve(f3, -3,3)

f4<- function(n){
result<- (1/2)^n
return(result)
}
curve(f4, -5,5)

