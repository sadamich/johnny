### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

n<- 200
x<- 1:200
set.seed(75)
e<- dnorm(200,0,1)
y_star<- -10+0.1*x+e

y<- c(1 if y_star >= 0  and 0 if y_star <0 )
