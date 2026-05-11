### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 1 13 (p.73)
xr113<- read.csv("xr113.csv", header=TRUE)
str(xr113)
attach(xr113)
y<- SALARY
x<- EDUC
### Problem (a)
### asymmetric/ skewness
hist(y)
hist(x)

### Problem (b)
skew<- function(y,mean,n){
result<- 1/n* (sum (y - mean)^3)
return(result)
}
skew(y,mean(y),474)

kurt<-  function(y,mean,n){
result<- 1/n*(sum(y - mean)^4)
return(result)
}
kurt(y,mean(y),474)

### Problem (c)

### Problem (d)
z<- log(y)
mean(z)
[1] 10.35679
log(mean(y))
[1] 10.44638
median(z)
[1] 10.27073
log(median(y))
[1] 10.27073

### Problem (e)

### Problem (f)

### Problem (g) 

mu<- exp(mean(z)+0.5*var(z))

mean(y)