https://de.wikipedia.org/wiki/Trigonometrische_Funktion

f_sin<- function(x){
result<- x - 1/factorial(3)*x^3 + 1/factorial(5)*x^5 - 1/factorial(7)*x^7 
          + 1/ factorial(9)*x^9
return(result)
}
curve(f_sin, -3,3)

curve(sin(x), add=TRUE, col="red")

https://de.wikipedia.org/wiki/Eulersche_Formel
f_euler<- function(x){
result<- cos(x)+ 1i*sin(x)
return(result)
}
curve(f_euler, -10,10)
Warning message:
In xy.coords(x, y, xlabel, ylabel, log) :
  imaginary parts discarded in coercion
