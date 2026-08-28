
exp(1i*pi)+1
[1] 0+1.224606e-16i

library(maxLik)
f1<- function(x){
result<- 1/(x-2)-1
return(result)
}

m1<- maxLik(f1, start=c(0))
summary(m1)
curve(f1, -5,5, xlab="x",ylab="f(x)")


f2<- function(x){
result<- 2/(x+1)-3
return(result)
}

m2<- maxLik(f2, start=c(0))
summary(m2)
curve(f2, -5,5, xlab="x",ylab="f(x)")

f3<- function(x){
result<- (x-1)/(x-2)
return(result)
}

m3<- maxLik(f3, start=c(0))
summary(m3)
curve(f3, -5,5, xlab="x",ylab="f(x)")

f4<- function(x){
result<- (3*x+1)/(x+1)
return(result)
}

m4<- maxLik(f4, start=c(0))
summary(m4)
curve(f4, -5,5, xlab="x",ylab="f(x)")

f5<- function(x){
result<- 3/(x+1)
return(result)
}

m5<- maxLik(f5, start=c(0))
summary(m5)
curve(f5, -5,5, xlab="x",ylab="f(x)")

f5a<- function(x){
result<- x-1
return(result)
}
curve(f5a, -5,5, xlab="x",ylab="f(x)",add= TRUE, col = "red")

f5b<- function(x){
result<- -3+0*x
return(result)
}
curve(f5b, -5,5, xlab="x",ylab="f(x)",add= TRUE, col = "blue")

f6<- function(x){
result<- sqrt(-2*x+4)
return(result)
}
curve(f6, -5,5, xlab="x",ylab="f(x)")

f6a<- function(x){
result<- -sqrt(3*x+3)
return(result)
}
curve(f6a, -5,5, xlab="x",ylab="f(x)")

f7<- function(x){
result<- sqrt(2*x+2)
return(result)
}
curve(f7, -5,10, xlab="x",ylab="f(x)")
f7a<- function(x){
result<- x-3
return(result)
}
curve(f7a, -5,10, xlab="x",ylab="f(x)",add=TRUE,col ="red")

f8<- function(x){
result<- -sqrt(x+1)
return(result)
}
curve(f8, -5,5, xlab="x",ylab="f(x)")
f8a<- function(x){
result<- x-1
return(result)
}
curve(f8a, -5,5, xlab="x",ylab="f(x)",add=TRUE,col ="red")