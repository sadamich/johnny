https://cran.r-project.org/web/packages/gmm/refman/gmm.html#bread
library(gmm)


# See \code{\link{gmm}} for more details on this example.
# With the identity matrix 
# bread is the inverse of (G'G)
### The observations
n <- 1000
### The random variable
x <- rnorm(n, mean = 4, sd = 2)
### The moment function: the moment conditions
g <- function(tet, x)
        {
        m1 <- (tet[1] - x)
        m2 <- (tet[2]^2 - (x - tet[1])^2)
        m3 <- x^3 - tet[1]*(tet[1]^2 + 3*tet[2]^2)
        f <- cbind(m1, m2, m3)
        return(f)
        }
Dg <- function(tet, x)
        {
        jacobian <- matrix(c( 1, 2*(-tet[1]+mean(x)), -3*tet[1]^2-3*tet[2]^2,0, 2*tet[2],
				-6*tet[1]*tet[2]), nrow=3,ncol=2)
        return(jacobian)
        }

res <- gmm(g, x, c(0, 0), grad = Dg,weightsMatrix=diag(3))
G <- Dg(res$coef, x)
bread(res)
solve(crossprod(G))

G
             [,1]       [,2]
[1,]   1.00000000   0.000000
[2,]  -0.01249264   4.064097
[3,] -59.72399158 -48.430840


https://cran.r-project.org/web/packages/gmm/refman/gmm.html#vcov

vcov(res)
    Theta[1]     Theta[2]
Theta[1] 0.0041078481 0.0001813442
Theta[2] 0.0001813442 0.0021745732


### The sandwich estimator 
B<- bread(res)
M<- meat(res)
t(B)%*%solve(M)%*%B