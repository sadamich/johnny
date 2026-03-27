### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 9 
x<- runif(100, 0, 20)
plot(x)
hist(x)
library(maxLik)
e<- rnorm(100, 0, 0.01)
f<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
beta3<- theta[3]
sigma<- theta[4]
y<- beta1+beta2*x^beta3+e
mu <- beta1+beta2*x^beta3
N<- 100

-N*0.5*log(pi)-N*0.5*log(sigma^2)-((y - mu)^2/sigma^2)
}
m<- maxLik(f, start = c(0,1,1,1))
summary(m)

Maximum Likelihood estimation
Newton-Raphson maximisation, 2 iterations
Return code 3: Last step could not find a value above the current.
Boundary of parameter space?  
Consider switching to a more robust optimisation method temporarily.
Log-Likelihood: 12345.72 
4  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)
[1,]   0.0000        Inf       0       1
[2,]   1.0000        Inf       0       1
[3,]   1.0000        Inf       0       1
[4,]  -0.1642        Inf       0       1