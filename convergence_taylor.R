https://de.wikipedia.org/wiki/Taylorreihe


f_t<- function(x){
result<- 1 + x + x^2/factorial(2)+ x^3/factorial(3) + x^4/factorial(4)
         + x^5/factorial(5) + x^6/factorial(6) + x^7/factorial(7)
         + x^8/factorial(8) + x^9/factorial(9) + x^10/factorial(10)
return(result)
}
curve(f_t, -4,4)


curve(exp(x), add = TRUE, col = "red")



f<- function(x){
result<- exp(- 1/x^2)
return(result)
}
curve(f, -4 ,4)