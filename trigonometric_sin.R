

f_sin<- function(x){
result<- x - 1/factorial(3)*x^3 + 1/factorial(5)*x^5 - 1/factorial(7)*x^7 
          + 1/ factorial(9)*x^9
return(result)
}
curve(f_sin, -3,3)

curve(sin(x), add=TRUE, col="red")