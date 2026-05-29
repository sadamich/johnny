### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Seite 197 - 198 
f<- function(x){
result<- x^4
return(result)
}
f(3)

curve(f(x), -3,3, xlab="x", ylab="f(x)")
### the gradient                                                            ###
g<- function(x){
result<- 4*x^3
return(result)
}
curve(g(x), -3,3, xlab="x",ylab="g(x)")

integrate(g, lower = -5, upper = 5)

h<- function(x){
result <- 12*x^2
return(result)
}
curve(h(x),-3,3, xlab= "x",ylab="h(x)")

### The Raphson Newton method

theta_h1 <- function(theta_0){
result<- theta_0 - 1/h(x)*g(x)
return(result)
}
x<- c(3)
theta_h1(0)
[1]  1.0000000  0.6666667  0.3333333  NaN -0.3333333 -0.6666667 -1.0000000

### 
x_neu <- 3
x_alt <- 0
while(abs(x_alt - x_neu) >= 1e-10){
x_alt<- x_neu
x_neu<- x_alt - x_alt/3
}
c(ergebnis = x_neu, differenz = abs(x_alt - x_neu))


### die Fehlersuche (Debugging)

iter<- 0
x_neu <- x_alt <- 5
repeat {
iter <- iter + 1
cat("Iteration:", iter, "\n")
x_alt <- x_neu
x_neu <- x_alt - x_alt/3
if(abs(x_alt - x_neu)< 1e-10)break
}
iter
c(ergebnis = x_neu, differenz= abs(x_alt - x_neu))

