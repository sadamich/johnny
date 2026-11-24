https://de.wikipedia.org/wiki/Fehlerfunktion

f<- function(x){
result<- 2/sqrt(pi)*exp(-x^2)
return(result)
}
curve(f, -10,10)

integrand <- function(x) {2/sqrt(pi)*exp(-x^2)}
integrate(integrand, lower = 0, upper = Inf)
