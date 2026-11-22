### 3.2 Estimatingtheparametersofastabledistribution
### The moment conditon of the characteristic function (the closed form)   ###
g2 <-function(theta,x)
{
tau<-seq(1,5,length.out=10)
pm <-1
x <-matrix(c(x),ncol=1)
x_comp <-x%*%matrix(tau,nrow=1)
x_comp <-matrix(complex(ima=x_comp),ncol=length(tau))
emp_car <-exp(x_comp)
the_car <-charStable(theta,tau,pm)
gt<-t(t(emp_car)-the_car)
gt <-cbind(Im(gt),Re(gt))
return(gt)
}
install.packages("stabledist")
library(stabledist)
set.seed(345)
x2<-rstable(500,1.5,.5,pm=1)
t0 <-c(alpha =2, beta= 0,gamma =sd(x2)/sqrt(2),delta =0)
print(res<-gmm(g2,x2,t0))
Method   twoStep 
Objective function value:  0.1095069 
  alpha     beta    gamma    delta  
 1.1606  -1.7842   1.2468   3.0203  
Convergence code =  1 
summary(res)

### the option function : nlminb
https://cran.r-project.org/doc/manuals/r-patched/packages/stats/refman/stats.html#topic+nlminb

res2 <-gmm(g2,x2,t0,optfct="nlminb",lower=c(0,-1,0,-Inf),upper=c(2,1,Inf,Inf))
summary(res2)

### Example for the estimation of parameter (a stable distribution)        ###
data(Finance)
str(Finance)
x3 <-Finance[1:1500,"WMK"]
str(x3)
t0<-c(alpha= 1.8, beta= 0.1, gamma= sd(x3)/sqrt(2),delta= 0)
res3 <-gmm(g2,x3,t0,optfct="nlminb")
summary(res3)

library(car) 
linearHypothesis(res3,cbind(diag(2),c(0,0),c(0,0)),c(2,0))

