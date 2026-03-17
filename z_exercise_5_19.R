### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
set.seed(200)
x<- runif(100, 0,20)
e<- rnorm(100,0, 0.01)
y<- 2+sqrt(x)+e

plot(y)
### right bias ###
hist(y)
summary(y)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.366   4.160   5.112   4.927   5.625   6.468 
eq<- lm(y ~ x)
summary(eq)