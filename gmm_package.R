### https://cran.r-project.org/web/packages/gmm/vignettes/gmm_with_R.pdf ###
library(gmm)
### The moment conditions  
g1 <- function(tet,x)
{
m1 <- (tet[1]-x)
m2 <- (tet[2]^2- (x- tet[1])^2)
m3 <- x^3-tet[1]*(tet[1]^2+3*tet[2]^2)
f <- cbind(m1,m2,m3)
return(f)
}

### Covariance matrix  Gradient  
Dg <- function(tet,x)
{
G <- matrix(c( 1,
2*(-tet[1]+mean(x)),-3*tet[1]^2-3*tet[2]^2,0,
2*tet[2],-6*tet[1]*tet[2]),
nrow=3,ncol=2)
return(G)
}

set.seed(123)
n<-200
x1 <-rnorm(n, mean= 4,sd = 2)
print(res<-gmm(g1,x1,c(mu = 0, sig= 0), grad= Dg))

Method
 twoStep 
Objective function value:  0.01307637 
    mu     sig  
3.8939  1.7867  
Convergence code =  0 

summary(res)
Call:
gmm(g = g1, x = x1, t0 = c(mu = 0, sig = 0), gradv = Dg)
Method:  twoStep 
Kernel:  Quadratic Spectral(with bw =  0.71322 )
Coefficients:
     Estimate     Std. Error   t value      Pr(>|t|)   
mu    3.8939e+00   1.2032e-01   3.2364e+01  8.9209e-230
sig   1.7867e+00   8.3472e-02   2.1405e+01  1.1937e-101
J-Test: degrees of freedom is 1 
                J-test   P-value
Test E(g)=0:    2.61527  0.10584

Initial values of the coefficients
      mu      sig 
4.022499 1.881766 
#############
Information related to the numerical optimization
Convergence code =  0 
Function eval. =  63 
Gradian eval. =  NA 


specTest(res)
 ##  J-Test: degrees of freedom is 1  ## 
                J-test   P-value
Test E(g)=0:    2.61527  0.10584


sim_ex <- function(n,iter)
{
tet1 <- matrix(0,iter,2)
tet2 <- tet1
for(i in 1:iter)
{
x1<- rnorm(n, mean= 4,sd =2)
tet1[i,1] <- mean(x1)
tet1[i,2] <- sqrt(var(x1)*(n-1)/n)
tet2[i,]<- gmm(g1,x1,c(0,0),grad=Dg)$coefficients
}
bias <- cbind(rowMeans(t(tet1)-c(4,2)),rowMeans(t(tet2)-c(4,2)))
dimnames(bias)<- list(c("mu","sigma"),c("ML","GMM"))
Var<- cbind(diag(var(tet1)),diag(var(tet2)))
dimnames(Var)<- list(c("mu","sigma"),c("ML","GMM"))
MSE <- cbind(rowMeans((t(tet1)-c(4,2))^2),rowMeans((t(tet2)-c(4,2))^2))
dimnames(MSE)<- list(c("mu","sigma"),c("ML","GMM"))
return(list(bias=bias,Variance=Var,MSE=MSE))
}

sim_ex(50, 2000)
$bias
                ML          GMM
mu     0.002257994 -0.008296646
sigma -0.028431398 -0.083807864

$Variance
              ML        GMM
mu    0.07270946 0.08820341
sigma 0.03885166 0.04522601

$MSE
              ML        GMM
mu    0.07267820 0.08822815
sigma 0.03964058 0.05222715


### the characteristic function   a stable distribution          ###

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
Method
 twoStep 
Objective function value:  0.1095069 

  alpha     beta    gamma    delta  
 1.1606  -1.7842   1.2468   3.0203  
Convergence code =  1 
summary(res)
Call:
gmm(g = g2, x = x2, t0 = t0)


Method:  twoStep 

Kernel:  Quadratic Spectral(with bw =  0.89393 )

Coefficients:
       Estimate     Std. Error   t value      Pr(>|t|)   
alpha   1.1606e+00   1.7482e-01   6.6385e+00   3.1679e-11
beta   -1.7842e+00   2.5618e-01  -6.9648e+00   3.2892e-12
gamma   1.2468e+00   8.1003e-02   1.5392e+01   1.8627e-53
delta   3.0203e+00   1.0181e+01   2.9666e-01   7.6672e-01

J-Test: degrees of freedom is 16 
                J-test      P-value   
Test E(g)=0:    5.4753e+01  3.9034e-06

Initial values of the coefficients
      alpha        beta       gamma       delta 
 0.99537936 -0.02472724  1.13127832  3.13327073 
#############
Information related to the numerical optimization
Convergence code =  1 
Function eval. =  501 
Gradian eval. =  NA 

res2 <-gmm(g2,x2,t0,optfct="nlminb",lower=c(0,-1,0,-Inf),upper=c(2,1,Inf,Inf))
summary(res2) 
Call:
gmm(g = g2, x = x2, t0 = t0, optfct = "nlminb", lower = c(0, 
    -1, 0, -Inf), upper = c(2, 1, Inf, Inf))


Method:  twoStep 

Kernel:  Quadratic Spectral(with bw =  0.89393 )

Coefficients:
       Estimate     Std. Error   t value      Pr(>|t|)   
alpha   6.9620e-01   1.0129e-01   6.8731e+00   6.2833e-12
beta   -4.8492e-01   1.4545e-01  -3.3339e+00   8.5649e-04
gamma   1.2090e+00   1.2961e-01   9.3279e+00   1.0797e-20
delta   6.2014e-01   4.2371e-01   1.4636e+00   1.4331e-01

J-Test: degrees of freedom is 16 
                J-test      P-value   
Test E(g)=0:    6.1215e+01  3.2621e-07

Initial values of the coefficients
        alpha          beta         gamma         delta 
 1.999998e+00  0.000000e+00  3.944987e+00 -8.940697e-08 

#############
Information related to the numerical optimization
Convergence code =  1 
Function eval. =  155 
Gradian eval. =  624 
Message:  iteration limit reached without convergence (10) 

### initialAlgoInfo’

data(Finance)
x3 <-Finance[1:1500,"WMK"]
t0<-c(alpha= 1.8, beta= 0.1, gamma= sd(x3)/sqrt(2),delta= 0)
res3 <-gmm(g2,x3,t0,optfct="nlminb")
summary(res3)
Call:
gmm(g = g2, x = x3, t0 = t0, optfct = "nlminb")


Method:  twoStep 

Kernel:  Quadratic Spectral(with bw =  0.80658 )

Coefficients:
       Estimate   Std. Error  t value    Pr(>|t|) 
alpha   1.908155   0.028692   66.504143   0.000000
beta    0.900367   0.414593    2.171689   0.029879
gamma   0.577074   0.013840   41.697298   0.000000
delta   0.069057   0.028372    2.434002   0.014933

J-Test: degrees of freedom is 16 
                J-test      P-value   
Test E(g)=0:    36.9139667   0.0021563

Initial values of the coefficients
    alpha      beta     gamma     delta 
1.6538600 0.3455514 0.5729315 0.1056138 

#############
Information related to the numerical optimization
Convergence code =  0 
Function eval. =  22 
Gradian eval. =  89 
Message:  relative convergence (4) 

library(car) 
linearHypothesis(res3,cbind(diag(2),c(0,0),c(0,0)),c(2,0))

Linear hypothesis test:
alpha = 2
beta = 0
Model 1: restricted model
Model 2: res3
  Df  Chisq Pr(>Chisq)    
1                         
2  2 22.238  1.483e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### linear iid moment conditions
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

Call:
gmm(g = g3, x = h)


Method:  twoStep 

Kernel:  Quadratic Spectral(with bw =  0.36504 )

Coefficients:
             Estimate   Std. Error  t value    Pr(>|t|) 
(Intercept)  -0.126831   0.090976   -1.394113   0.163283
w             0.329674   0.135113    2.439992   0.014688

J-Test: degrees of freedom is 2 
                J-test    P-value 
Test E(g)=0:    4.734496  0.093738

Initial values of the coefficients
(Intercept)           w 
-0.06989787  0.23510008 

res2 <-gmm(g3,x=h,type='iterative',crit=1e-8,itermax=200)
coef(res2)


res3 <-gmm(g3,x=h,res2$coef,type='cue')
coef(res3)

confint(res3,level=.90)
Wald type confidence interval
#######################################
             0.05       0.95     
(Intercept)  -0.280550   0.018335
w             0.112410   0.556209

plot(w,y,main="LSvs GMMestimation")
lines(w,fitted(res),col=2)
lines(w,fitted(lm(y~w)),col=3,lty=2)
lines(w,.1*w,col=4,lty=3)
legend("topleft",c("Data","Fitted GMM","Fitted LS","Trueline"),pch=c(1,NA,NA,NA),col=1:3,lty=c(NA,1,2,3))


### AR ARMA
t <-400
set.seed(345)
x5<-arima.sim(n=t,list(ar=c(1.4,-0.6),ma=c(0.6,-0.3)))
x5t<-cbind(x5)
for(i in 1:6) x5t<-cbind(x5t,lag(x5,-i))
x5t<-na.omit(x5t)
g4<-x5t[,1]~x5t[,2]+x5t[,3]
res<-gmm(g4,x5t[,4:7])
summary(res)
Call:
gmm(g = g4, x = x5t[, 4:7])


Method:  twoStep 

Kernel:  Quadratic Spectral(with bw =  2.13425 )

Coefficients:
             Estimate     Std. Error   t value      Pr(>|t|)   
(Intercept)  -1.0341e-01   9.9513e-02  -1.0391e+00   2.9874e-01
x5t[, 2]      1.2487e+00   1.2515e-01   9.9780e+00   1.9032e-23
x5t[, 3]     -5.1032e-01   9.8712e-02  -5.1698e+00   2.3437e-07

J-Test: degrees of freedom is 2 
                J-test   P-value
Test E(g)=0:    0.26575  0.87558

Initial values of the coefficients
(Intercept)    x5t[, 2]    x5t[, 3] 
 -0.1000513   1.2544985  -0.5136757 

res2 <-gmm(g4,x=x5t[,4:7],kernel="Truncated")
coef(res2)

diag(vcov(res2))^.5

plot(res,which=2)

plot(res,which=3)


### CAPM 

data(Finance)
r <-Finance[1:500,1:5]
rm <-Finance[1:500,"rm"]
rf <-Finance[1:500,"rf"]
z <-as.matrix(r-rf)
zm <-as.matrix(rm-rf)
res<-gmm(z~zm,x=zm)
coef(res)

test <-paste(names(coef(res)[1:5])," = 0",sep="")
linearHypothesis(res,test)
Linear hypothesis test:
WMK_((Intercept) = 0
UIS_((Intercept) = 0
ORB_((Intercept) = 0
MAT_((Intercept) = 0
ABAX_((Intercept) = 0

Model 1: restricted model
Model 2: z ~ zm

  Res.Df Df  Chisq Pr(>Chisq)
1    503                     
2    498  5 0.6432     0.9859


res2<-gmm(z~zm-1,cbind(1,zm))
specTest(res2)
 ##  J-Test: degrees of freedom is 5  ## 

                J-test   P-value
Test E(g)=0:    0.64322  0.98594

### Discounting factor 
g5 <- function(tet, x) {
gmat <- (tet[1] + tet[2] * (1 + c(x[, 1]))) * (1 + x[, 2:6])- 1
return(gmat)
}
res_sdf <- gmm(g5, x = as.matrix(cbind(rm, r)), c(0, 0))
specTest(res_sdf)
##  J-Test: degrees of freedom is 3  ## 

                J-test   P-value
Test E(g)=0:    0.60775  0.89466

### Approximation discret

g6 <- function(theta, x) {
t <- length(x)
et1 <- diff(x)- theta[1]- theta[2] * x[-t]
ht <- et1^2- theta[3] * x[-t]^(2 * theta[4])
g <- cbind(et1, et1 * x[-t], ht, ht * x[-t])
return(g)
}

rf <- Finance[,"rf"]
rf <- ((1 + rf/100)^(365)- 1) * 100
dr <- diff(rf)
res_0 <- lm(dr ~ rf[-length(rf)])
tet0 <- c(res_0$coef, var(residuals(res_0)), 0)
names(tet0) <- c("alpha", "beta", "sigma^2", "gamma")
res_rf <- gmm(g6, rf, tet0, control = list(maxit = 1000, reltol = 1e-10))
coef(res_rf)

### Panel data
y <- rbind(y1 - mean(y1),y2 - mean(y2),y3 - mean(y3))
x <- rbind(x1 - mean(x1),x2 - mean(x2),x3 - mean(x3))
res <- gmm(y~x,h)
y <- rbind(y1,y2,y3)
x <- rbind(x1,x2,x3)
res <- gmm(y~x,h)


### Sandwich 

gt <- g(t0, x)
V <- kernHAC(lm(gt~1), sandwich = FALSE)
W <- solve(V)