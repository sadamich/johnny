https://de.wikipedia.org/wiki/Satz_von_Rolle
https://de.wikipedia.org/wiki/Mittelwertsatz_der_Differentialrechnung
par(mfrow=c(2,2))
f<- function(x){
result<- sin(x)
return(result)
}
curve(f, -5,5)


c<- function(x){
result<- 0 + x*0
return(result)
}
curve(c, add = TRUE, col="red")

### f(x= 0) = f(x=pi) = 0  ###
sin(pi)
[1] 0
sin(0)
[1] 0
### between x= 0 and x= pi, D(sinx) = 0  ###
sin(pi/2)
[1] 1              D(sinx) = 0, 

