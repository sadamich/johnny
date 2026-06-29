### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 9 (p. 527)                                                  ###
set.seed(69)
x<- rnorm(10,100,100)
e<- rnorm(10, 0, 1)
y<- -10+0.1*x+e
str(y)
plot(y)
hist(y)
y_c<- ifelse(y < 0, 0, 1)
y_0<- y[y_c<=0]
str(y_0)
y_1<- y[y_c>0]
str(y_1)

### Problem (a) The truncated model: ML estimate 

### Problem (b) Comparision with OLS (bias), the marginal effects             ###
eq<- lm(y~x)
summary(eq)

### Problem (c) The censored model

### Problem (d) Comparision with OLS (bias), 

### Problem (e) Comparision of the truncated model with the censored



