https://cran.r-project.org/web/packages/urca/refman/urca.html#ablrtest
library(urca)
library(vars)
Likelihood ratio test for restrictions on alpha and beta
Description
This function estimates a restricted VAR, where the restrictions are based upon 
α, i.e. the loading vectors and 
β, i.e the matrix of cointegration vectors

data(denmark)
sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
sjd.vecm <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
HD1 <- matrix(c(1, -1, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 1), c(5,3))
DA <- matrix(c(1,0,0,0, 0, 1, 0, 0, 0, 0, 0, 1), c(4,3))
summary(ablrtest(sjd.vecm, H=HD1, A=DA, r=1))
###################### 
# Johansen-Procedure # 
###################### 

Estimation and testing under linear restrictions on alpha and beta 

The VECM has been estimated subject to: 
beta=H*phi and/or alpha=A*psi

     [,1] [,2] [,3]
[1,]    1    0    0
[2,]   -1    0    0
[3,]    0    1    0
[4,]    0   -1    0
[5,]    0    0    1


     [,1] [,2] [,3]
[1,]    1    0    0
[2,]    0    1    0
[3,]    0    0    0
[4,]    0    0    1

Eigenvalues of restricted VAR (lambda):
[1] 0.4100 0.0090 0.0053

The value of the likelihood ratio test statistic:
2.13 distributed as chi square with 2 df.
The p-value of the test statistic is: 0.35 

Eigenvectors, normalised to first column
of the restricted VAR:

        [,1]
[1,]  1.0000
[2,] -1.0000
[3,]  5.9508
[4,] -5.9508
[5,] -6.2162
Weights W of the restricted VAR:

        [,1]
[1,] -0.1519
[2,]  0.0992
[3,]  0.0000
[4,]  0.0288

