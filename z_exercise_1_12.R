### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 1 12 (p.73)                                                   ###
xr111<- read.csv("xr111.csv", header=TRUE)
str(xr111)
attach(xr111)

### Problem (a)                                                            ###
t<- function(mean, mu, sd){
result<- (mean - mu)/sd
return(result)
}
t(mean(FGPA), 2.79,sd(FGPA))
[1] -0.1875647

### Problem (b)

### Problem (c)

chi<- function(s_sq, sigma_s,n){
result<- (n-1)*s_sq/sigma_s
return(result)
}
chi(0.2911111, 0.2116,10)
[1] 12.38185

pchisq(12.38185, 9)
[1] 0.8073718
P-value
1 - pchisq(12.38185, 9)
[1] 0.1926282

### Problem (d)

p<- 6/10
var(FEM)
1/10*p*(1-p)
0.024


236/609