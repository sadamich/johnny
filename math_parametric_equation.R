https://de.wikipedia.org/wiki/Parameter_(Mathematik)
https://de.wikipedia.org/wiki/Parameterdarstellung
f<- function(x){
result <- x
return(x)
}

curve(f, -10, 10)
f1<- function(x){
p<- 1
result<- x +p
return(result)
}
curve(f1,add=TRUE, col="red", -10,10)


f2<- function(x){
p<-3
result<- p*x
return(result)
}
curve(f2, add = TRUE, col="blue")
