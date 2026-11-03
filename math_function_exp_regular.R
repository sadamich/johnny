### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 1 9 and Exercise 1 10 (p 72 - 73)                             ###
f<- function(x){
result<- exp(-1/2*x^2)
return(result)
}
curve(f, -5,5)
### Excersice 1 9 (d) (p.72) cf) Laplace distribution
f2<- function(x){
result<- 1/2*exp(-abs(x))
return(result)
}
curve(f2, -2,2)

### Exercise 1 10   cf) the moment estimator 
f3<- function(x){
result<- exp(x)
return(result)
}
curve(f3, 0.1, 5)

