### 3 3 linear iid moment conditions
### the instrumental variables 
library(mvtnorm)
set.seed(112233)
sig <- matrix(c(1,.5,.5,1),2,2)
n <- 400
e <- rmvnorm(n,sigma=sig)
x4 <- rnorm(n)
w <- exp(-x4^2) + e[,1]
y <- 0.1*w + e[,2]
h <- cbind(x4, x4^2, x4^3)
g3 <- y~w
summary(res<-gmm(g3,x=h))

res2 <-gmm(g3,x=h,type='iterative',crit=1e-8,itermax=200)
coef(res2)

res3 <-gmm(g3,x=h,res2$coef,type='cue')
coef(res3)

confint(res3,level=.90)

plot(w,y,main="LSvs GMMestimation")
lines(w,fitted(res),col=2)
lines(w,fitted(lm(y~w)),col=3,lty=2)
lines(w,.1*w,col=4,lty=3)
legend("topleft",c("Data","Fitted GMM","Fitted LS","Trueline"),pch=c(1,NA,NA,NA),col=1:3,lty=c(NA,1,2,3))
