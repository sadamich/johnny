### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### 5 2 3 Non parametrich estimation (p.289)                               ###                            
w<- function(d){
result<- (1-d^3)^3
return(result)
}

w(0)
[1] 1
w(0.2)
[1] 0.9761915
w(0.4)
[1] 0.8200259
w(0.6)
[1] 0.4818903
w(0.8)
[1] 0.1162143
w(1)
[1] 0
w(xbw)
curve(w, 0, 1, lty=2, ylim=c(0, 1), xlab = "d", ylab="Weight")

### Example 5 3 (p. 293)                                                   ###
set.seed(35)
x<- runif(200, 0, 2.5)
e<- rnorm(200, 0, 0.04)
y<- sin(x)+e

plot(x)
xbw<- 0.6*x
ybw<- 0.6*y
plot(x,y)
plot(xbw,ybw)
data_bw<- data.frame(xbw, ybw)
eq_locfit<- locfit(ybw ~ xbw, data=data_bw)
plot(eq_locfit)



eq_w<- lm(y_w~x_w)
summary(eq_w)
const<- 0.682819 
eq_f<- lm(y~const)

library(sandwich)
curve(kweights(x, kernel = "Quadratic", normalize = TRUE),
      from = 0, to = 3.2, xlab = "x", ylab = "k(x)")
curve(kweights(x, kernel = "Bartlett", normalize = TRUE),
      from = 0, to = 3.2, col = 2, add = TRUE)
curve(kweights(x, kernel = "Parzen", normalize = TRUE),
      from = 0, to = 3.2, col = 3, add = TRUE)
curve(kweights(x, kernel = "Tukey", normalize = TRUE),
      from = 0, to = 3.2, col = 4, add = TRUE)
curve(kweights(x, kernel = "Truncated", normalize = TRUE),
      from = 0, to = 3.2, col = 5, add = TRUE)


library(locfit)
# fit and plot a univariate local regression
data(ethanol, package="locfit")
str(ethanol)
'data.frame':   88 obs. of  3 variables:
 $ NOx: num  3.74 2.29 1.5 2.88 0.76 ...
 $ C  : num  12 12 12 12 12 9 9 9 12 12 ...
 $ E  : num  0.907 0.761 1.108 1.016 1.189 ...
attach(ethanol)
plot(E,NOx)
fit <- locfit(NOx ~ E, data=ethanol)
plot(fit, get.data=TRUE)

# a bivariate local regression with smaller smoothing parameter
fit <- locfit(NOx~lp(E,C,nn=0.5,scale=0), data=ethanol)
plot(fit)
### locfit(NOx ~ E, data=ethanol) ###
# density estimation
str(geyser)
num [1:107] 4.37 3.87 4 4.03 3.5 4.08 2.25 4.7 1.73 4.93 ...
plot(geyser)
data(geyser, package="locfit")
fit <- locfit( ~ lp(geyser, nn=0.1, h=0.8))
plot(fit,get.data=TRUE)





library(maxLik)
w<- function(d){
result<- (1-d^3)^3
return(result)
}
w_sp<- w(0.6)
x0<- 1.5
fn<- function(theta){
a<- theta[1]
b<- theta[2]
e<- y - a - b*(x - x0)
-w_sp*crossprod(e)
}
summary(maxBFGS(fn, start=c(0,0)))
BFGS maximization 
Number of iterations: 80 
Return code: 0 
successful convergence  
Function value: -3.970968e+15 
Estimates:
      estimate gradient
[1,] -128357.1        0
[2,] -405211.8  1500000
