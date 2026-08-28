f1<- function(x){
result<- sqrt(8*x)
return(result)
}
curve(f1, -1,5, xlab="x",ylab="f(x)", xlim= c(-1, 5), ylim=c(-5, 5))

f1a<- function(x){
result<- -sqrt(8*x)
return(result)
}
curve(f1a, -1,5, xlab="x",ylab="f(x)", add=TRUE, col="red")
f2<- function(x){
result<- -sqrt(4*x)
return(result)
}
curve(f2, -5,5, xlab="x",ylab="f(x)")

f3<- function(x){
result<- x^2/4
return(result)
}
curve(f3, -5,5, xlab="x",ylab="f(x)")

f4<- function(x){
result<- -2*x^2
return(result)
}
curve(f4, -5,5, xlab="x",ylab="f(x)")