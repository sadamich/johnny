f<- function(x){
result<- exp(-1/2*x^2)
return(result)
}
curve(f, -5,5)

f2<- function(x){
result<- exp(-1/2*x)
return(result)
}
curve(f2, -2,2)
f2_p<- function(x){
result<- exp(1/2*x)
return(result)
}
curve(f2_p, -2,2)

f2_d<- function(x){
result<- exp(-1/2*x)-exp(-1/2*x)
return(result)
}
curve(f2_d, -2,2)