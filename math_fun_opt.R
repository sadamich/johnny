f<- function(x){
result<- x^2+2*x+1
return(result)
}

curve(f(x), -5,5, xlab="x",yalb="f(x)")
optimize(f, c(-5,5))

$minimum
[1] -1
$objective
[1] 0


f1<- function(x){
result<- x^2+5*x+6
return(result)
}

curve(f1(x),-5,5,xlab="x",ylab="f(x)")
optimize(f1,c(-5,5))
$minimum
[1] -2.5
$objective
[1] -0.25

