https://de.wikipedia.org/wiki/Potenzreihe

f_log<- function(x){
result<- x - x^2/2 + x^3/3 - x^4 + x^5/5 - x^6/6 + x^7/7 - x^8/8
       + x^9/9 - x^10/10
return(result)
}
curve(f_log, -1, 1)

curve(log(x), add =TRUE, col ="red")