https://cran.r-project.org/web/packages/gmm/index.html
### https://cran.r-project.org/web/packages/gmm/vignettes/gmm_with_R.pdf ###
library(gmm)
### 3 1 The moment conditions  
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

### ML estimation                                                          ###
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


























