https://de.wikipedia.org/wiki/Tabelle_von_Ableitungs-_und_Stammfunktionen

### Integration and normal distribution                                   ###
integrand <- function(x) {1/sqrt(2*pi)*exp(-0.5*x^2)}
## don't do this if you really want the integral from 0 to Inf
integrate(integrand, lower = -Inf, upper = Inf)


### The exponential function                                                 ###
integrand <- function(x) {exp(x)}
integrate(integrand, lower = 0, upper = 2)
6.389056 with absolute error < 7.1e-14  (1)
### The exponential function * k 
integrand2<- function(x,k){
result<- exp(k*x)
return(result)
}
integrate(integrand2, k= 2,lower = 0, upper = 2)
26.79908 with absolute error < 3e-13

integrate(integrand2, k= 1,lower = 0, upper = 2)
6.389056 with absolute error < 7.1e-14 (2)  ### (1) = (2)

### 
integrand3 <- function(x,a) {a^x}
integrate(integrand3, a= 2,lower = 0, upper = 2)
4.328085 with absolute error < 4.8e-14


### 
integrand4 <- function(x) {x^x*(1+log(x))}
integrate(integrand4,lower = 0, upper = 2)
3 with absolute error < 4.4e-06