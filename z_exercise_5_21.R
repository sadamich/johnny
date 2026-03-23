### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
set.seed(200)
x<- runif(100, 0,20)
e<- rnorm(100,0, 0.01)
y<- 2+sqrt(x)+e

plot(x)
plot(y)
hist(y)
acf(y)
eq<- lm(y~x)
res<- resid(eq)
acf(res)
x_sort<- sort(x)
plot(x_sort)
eq_sort<- lm(y~ x_sort)
res_s<- resid(eq_sort)
acf(res_s)

### Problem (c)
y_c<- 2+ x+ e
eq_c<- lm(y_c ~ x)
summary(eq_c)
res_c <- resid(eq_c)
acf(res_c)
plot(res_c)

eq_c_sort<- lm(y_c ~ x_sort)
res_c_sort<- resid(eq_c_sort)
acf(res_c_sort)
plot(res_c_sort)