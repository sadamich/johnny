### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 8 (p. 527)

x<- rnorm(10, 100, 100)
e<- rnorm(10, 0, 1)
y<- -10 + 0.1*x+e


x2<- rnorm(100, 100, 100)
e2<- rnorm(100, 0, 1)
y2<- -10 + 0.1*x2+e2

x3<- rnorm(1000, 100, 100)
e3<- rnorm(1000, 0, 1)
y3<- -10 + 0.1*x3+e3

### Problem (a) The Probit model 

### Problem (b) The Logit model

### Problem (c) The heteroskedasticity

### Problem (d) the heteroskedasticity
set.seed(100)
x<- rnorm(100, 100,100)
e_sp<- rnorm(100, 0, exp(x/100))


### Problem (e) the inconsistence and no more Prbit 

### Problem (f) the model adjustment
