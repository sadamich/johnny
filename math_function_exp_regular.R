### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

f<- function(x){
result<- exp(-1/2*x^2)
return(result)
}
curve(f, -5,5)

f2<- function(x){
result<- exp(-1/2*x)
return(result)
}
curve(f2, -2,2)
f2_p<- function(x){
result<- exp(1/2*x)
return(result)
}
curve(f2_p, -2,2)

f2_d<- function(x){
result<- exp(-1/2*x)-exp(-1/2*x)
return(result)
}
curve(f2_d, -2,2)