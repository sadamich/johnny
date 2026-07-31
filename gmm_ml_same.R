library(gmm)
### The GMM estimation                                                     ###
g1 <- function(tet,x)
{
m1 <- (tet[1]-x)
m2 <- (tet[2]^2- (x- tet[1])^2)
f <- cbind(m1,m2)
return(f)
}

Dg <- function(tet,x)
{
G <- matrix(c( 1,
2*(-tet[1]+mean(x)),0,
2*tet[2]),
nrow=2,ncol=2)
return(G)
}

set.seed(123)
n<-30
x1 <-rnorm(n, mean= 0,sd = 1)
print(res<-gmm(g1,x1,c(mu = 0, sig= 0), grad= Dg))
Method
 twoStep 
Objective function value:  2.670345e-09 
       mu        sig  
-0.047144   0.964559  
Convergence code =  0 

### The ML estimation                                                      ###
library(maxLik)
 loglik <- function(theta) {
mu <- theta[1]
sigma <- theta[2]
sum(dnorm(x1, mean=mu, sd=sigma, log=TRUE))
}
 m <- maxLik(loglik, start=c(mu=1, sigma=2))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 7 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -41.48509 
2  free parameters
Estimates:
      Estimate Std. error t value  Pr(> t)    
mu     -0.0471     0.1761  -0.267    0.789    
sigma   0.9645     0.1245   7.746 9.47e-15 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 

### ML and GMM are the same                                                ###