https://cran.r-project.org/web/packages/gmm/refman/gmm.html#gel
library(gmm)
# First, an exemple with the fonction g()
g <- function(tet, x)
	{
	n <- nrow(x)
	u <- (x[7:n] - tet[1] - tet[2]*x[6:(n-1)] - tet[3]*x[5:(n-2)])
	f <- cbind(u, u*x[4:(n-3)], u*x[3:(n-4)], u*x[2:(n-5)], u*x[1:(n-6)])
	return(f)
	}

Dg <- function(tet,x)
	{
	n <- nrow(x)
	xx <- cbind(rep(1, (n-6)), x[6:(n-1)], x[5:(n-2)])
        H  <- cbind(rep(1, (n-6)), x[4:(n-3)], x[3:(n-4)], x[2:(n-5)], x[1:(n-6)])
	f <- -crossprod(H, xx)/(n-6)
	return(f)
	}
n = 200
phi<-c(.2, .7)
thet <- 0.2
sd <- .2
set.seed(123)
x <- matrix(arima.sim(n = n, list(order = c(2, 0, 1), ar = phi, ma = thet, sd = sd)), ncol = 1)

res <- gel(g, x,c(0, .3, .6), grad = Dg)
summary(res)
Call:
gel(g = g, x = x, tet0 = c(0, 0.3, 0.6), gradv = Dg)

Type of GEL:  EL 
Coefficients:
          Estimate  Std. Error  t value  Pr(>|t|)
Theta[1]  0.00404   0.06824     0.05913  0.95285 
Theta[2]  0.30265   0.14521     2.08425  0.03714 
Theta[3]  0.56867   0.13973     4.06972  0.00005 

Lambdas:
         Estimate  Std. Error  t value   Pr(>|t|)
Lam(g1)  -0.00585   0.00461    -1.26860   0.20458
Lam(g2)   0.09254   0.05330     1.73612   0.08254
Lam(g3)   0.08561   0.03862     2.21675   0.02664
Lam(g4)  -0.14497   0.09421    -1.53878   0.12386
Lam(g5)  -0.06443   0.07631    -0.84429   0.39850

 Over-identifying restrictions tests: degrees of freedom is 2 
         statistics  p-value
LR test  3.97759     0.13686
LM test  3.95528     0.13840
J test   3.95528     0.13840

Convergence code for the coefficients:  0 
Convergence code for the lambdas:  0 


# The same model but with g as a formula....  much simpler in that case
y <- x[7:n]
ym1 <- x[6:(n-1)]
ym2 <- x[5:(n-2)]

H <- cbind(x[4:(n-3)], x[3:(n-4)], x[2:(n-5)], x[1:(n-6)])
g <- y ~ ym1 + ym2
x <- H

res <- gel(g, x, c(0, .3, .6))
summary(res)
Call:
gel(g = g, x = x, tet0 = c(0, 0.3, 0.6))

Type of GEL:  EL 

Coefficients:
             Estimate  Std. Error  t value  Pr(>|t|)
(Intercept)  0.00404   0.06824     0.05913  0.95285 
ym1          0.30265   0.14521     2.08425  0.03714 
ym2          0.56867   0.13973     4.06972  0.00005 

Lambdas:
                  Estimate  Std. Error  t value   Pr(>|t|)
Lam((Intercept))  -0.00585   0.00461    -1.26860   0.20458
Lam(h1)            0.09254   0.05330     1.73612   0.08254
Lam(h2)            0.08561   0.03862     2.21675   0.02664
Lam(h3)           -0.14497   0.09421    -1.53878   0.12386
Lam(h4)           -0.06443   0.07631    -0.84429   0.39850

 Over-identifying restrictions tests: degrees of freedom is 2 
         statistics  p-value
LR test  3.97759     0.13686
LM test  3.95528     0.13840
J test   3.95528     0.13840

Convergence code for the coefficients:  0 
Convergence code for the lambdas:  0 

# Using evalGel to create the object without estimation

res <- evalGel(g, x, res$coefficients)
res
Type de GEL:  EL (Eval only, tests non-valid)  

Coefficients:
(Intercept)          ym1          ym2  
  0.0040355    0.3026497    0.5686680  

Lambdas:
Lam((Intercept))           Lam(h1)           Lam(h2)           Lam(h3)  
      -0.0058512         0.0925428         0.0856086        -0.1449731  
         Lam(h4)  
      -0.0644280  
Convergence code for the coefficients:  
Convergence code for Lambda:  0 
