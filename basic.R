### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm101<- read.csv("xm101.csv", header=TRUE)
str(xm101)
attach(xm101)
### Exhibit 1 5 (p.19)
var(FGPA)
cov(FGPA,SATM)
cov(FGPA,SATV)
cov(FGPA,FEM)
var(SATM)
cov(SATM,SATV)
cov(SATM,FEM)
var(SATV)
cov(SATV,FEM)
var(FEM)
r1<- c(0.2118186,0.05347879,0.02852227, 0.03960535)
r2<- c(0.05347879,0.3549359,0.1152905, -0.04725634)
r3<- c(0.02852227,0.1152905,0.4521184,  0.01100812)
r4<- c(0.03960535,-0.04725634,0.01100812,0.2377387)
test_cov<- rbind(r1,r2,r3,r4)
test_cov

cor(FGPA,SATM)
cor(FGPA,SATV)
cor(FGPA,FEM)

cor(SATM,SATV)
cor(SATM,FEM)

cor(SATV,FEM)
cor(FEM)
r1c<- c(1,    0.1950404, 0.09216712, 0.1764907)
r2c<- c(0.1950404,   1,  0.2878011, -0.1626804)
r3c<- c(0.09216712,0.2878011,  1  , 0.03357664)
r4c<- c(0.1764907, -0.1626804, 0.03357664, 1  )
test_cor<- rbind(r1c,r2c,r3c,r4c)
test_cor

### Exhibit 1 6 (p.28)
mean(FGPA)
[1] 2.792796
373/609*mean(FGPA[FEM==0])+236/609*mean(FGPA[FEM==1])
[1] 2.792796
var(FGPA)
[1] 0.2118186
373/609*var(FGPA[FEM==0])+236/609*var(FGPA[FEM==1])
[1] 0.2055691

### Exhibit 1 2 (p.14)
hist(FGPA)

x<- FGPA
summary(x)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.500   2.485   2.773   2.793   3.116   3.971 
skew<- function(y,mean,n,sd){
result<- 1/n*(sum ((y - mean)^3))/sd^3
return(result)
}
skew(x,mean(x),609,sd(x))
[1] 0.1674154
kurt<-  function(y,mean,n,sd){
result<- (1/n*(sum ((y - mean)^4)))/sd^4
return(result)
}
kurt(x,mean(x),609,sd(x))
[1] 2.502284

library(REdaS)
densbox(x~1, main="FGPA")
mean(x)
[1] 2.792796
sd(x)
[1] 0.4602375

### Probability density function (as a random variable) 
curve(dnorm(x, 2.792796,0.4602375),from = 0, to= 5)
dnorm(1, 2.792796,0.4602375)

### Exhibit 1 2 (p.14)
plot(ecdf(x))

plot(SATM, FGPA)
plot(SATV,FGPA)
plot(SATV,SATM)

hist(SATM)
plot(ecdf(SATM))
plot(ecdf(SATV))

u<- c(101:508)
fgpa<- FGPA[-u]
hist(fgpa)
summary(fgpa)
skew(fgpa,mean(fgpa),201,sd(fgpa))
kurt(fgpa,mean(fgpa),201,sd(fgpa))
fgpa_m<- FGPA[201:400]
hist(fgpa_m)
summary(fgpa_m)
skew(fgpa_m,mean(fgpa_m),200,sd(fgpa_m))
kurt(fgpa_m,mean(fgpa_m),200,sd(fgpa_m))

z0<- FGPA[FEM==0]
summary(z0)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.500   2.436   2.688   2.728   3.003   3.948 
skew(z0, mean(z0), 373, sd(z0))
[1] 0.2168447
kurt(z0, mean(z0), 373, sd(z0))
[1] 2.644018
z1<- FGPA[FEM==1]
summary(z1)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.805   2.569   2.897   2.895   3.210   3.971 
skew(z1, mean(z1), 236, sd(z1))
[1] 0.0341061
kurt(z1, mean(z1), 236, sd(z1))
[1] 2.333425

