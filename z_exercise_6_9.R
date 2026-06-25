### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 9 (p. 527)                                                  ###
x<- rnorm(10,100,100)
e<- rnorm(10, 0, 1)
y<- -10+0.1*x+e
str(y)
y_0<- y[y<=0]
str(y_0)
y_1<- y[y>0]
str(y_1)

hist(y)
hist(y_0)
hist(y_1)

### The truncated model: ML estimate 