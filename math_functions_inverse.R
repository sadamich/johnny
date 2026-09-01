### The inverse function
### the f (x) 
f_exp<- function(x){
result<- exp(x)
return(result)
}
f_exp(0)
[1] 1
exp(0)
[1] 1
### the f-1 (x)
f_ln<- function(x){
result<- log(x)
return(result)
}
f_ln(1)
[1] 0
log(1)
[1] 0

curve(exp(x), -1,3)
curve(log(x),-1,3, add=TRUE,col="red")