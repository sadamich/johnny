https://de.wikipedia.org/wiki/Kerndichtesch%C3%A4tzer
par(mfrow=c(2,2))
gauss<- function(t){
result<- 1/sqrt(2*pi)*exp(-1/2*t^2)
return(result)
}
curve(gauss, -5,5)

cauchy<- function(t){
result<- 1/pi*(1+t^2)
return(result)
}
curve(cauchy, -5,5)

picard<- function(t){
result<- 1/2*exp(-abs(t))
return(result)
}
curve(picard, -5,5)

epanechnikov<- function(t){
result<- 3/4*(1-t^2)
return(result)
}
curve(epanechnikov, -1,1)