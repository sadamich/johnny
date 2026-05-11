### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 1 15 (p.74)

### Problem (a)
set.seed(30)
x<- rnorm(10, 0,1)
x_s<- sample(x,5)
x_s
median(x_s)
### Problem (b)


### Problem (c)
set.seed(40)
z<- rt(1000,1)
z1<- sample(z, 1000)
plot(z1, type="l")
hist(z1)
mean(z1)


### Problem (d)

### Problem (e)

### problem (f)


### Problem (g)


