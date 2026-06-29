### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 8 (p. 527)
set.seed(10)
x<- rnorm(10, 100, 100)
e<- rnorm(10, 0, 1)
y<- -10 + 0.1*x+e
y_c<- ifelse(y <0, 0, 1)
str(y_c)

set.seed(100)
x2<- rnorm(100, 100, 100)
e2<- rnorm(100, 0, 1)
y2<- -10 + 0.1*x2+e2
set.seed(1000)
x3<- rnorm(1000, 100, 100)
e3<- rnorm(1000, 0, 1)
y3<- -10 + 0.1*x3+e3

### Problem (a) The Probit model 
eq_probit<- glm(formula = y_c ~ x, family = binomial(link = "probit"))
summary(eq_probit)
### Problem (b) The Logit model
eq_logit<- glm(formula = y_c~x, family = binomial)
summary(eq_logit)
### Problem (c) The heteroskedasticity
set.seed(10)
x<- rnorm(10, 100, 100)
e<- rnorm(10, 0, exp(x))
y<- -10 + 0.1*x+e
y_c_h<- ifelse(y< 0, 0,1)
eq_probit_h<- glm(formula = y_c_h ~ x, family = binomial(link = "probit"))
summary(eq_probit_h)
### Problem (d) the heteroskedasticity
set.seed(100)
x<- rnorm(10, 100,100)
e_sp<- rnorm(10, 0, exp(x/100))
y<- -10 + 0.1*x+e_sp
y_c_h2<- ifelse(y< 0, 0,1)
eq_probit_h2<- glm(formula = y_c_h2 ~ x, family = binomial(link = "probit"))
summary(eq_probit_h2)

### Problem (e) the inconsistence and no more Prbit 

### Problem (f) the model adjustment
