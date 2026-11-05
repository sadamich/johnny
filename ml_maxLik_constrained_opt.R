https://cran.r-project.org/web/packages/maxLik/vignettes/using-maxlik.pdf
library(maxLik)
### Constrained optimization
###
## We maximize exp(-x^2 - y^2) where x+y = 1
hatf <- function(theta) {
  x <- theta[1]
  y <- theta[2]
  exp(-(x^2 + y^2))
  ## Note: you may prefer exp(- theta %*% theta) instead
}
## use constraints: x + y = 1
A <- matrix(c(1, 1), 1, 2)
B <- -1
res <- maxNR(hatf, start=c(0,0), constraints=list(eqA=A, eqB=B),
             control=list(printLevel=1))
print(summary(res))

Newton-Raphson maximisation 
Number of iterations: 1 
Return code: 1 
gradient close to zero (gradtol) 
Function value: 0.6065399 
Estimates:
      estimate   gradient
[1,] 0.4999848 -0.6065307
[2,] 0.4999848 -0.6065307

Constrained optimization based on SUMT 
Return code: 1 
penalty close to zero 
5  outer iterations, barrier value 9.196987e-10 

f<- function(x){
result<- -x^2
return(result)
}
m<- maxNR(f, start = 0)
summary(m)

f<- function(x){
result<- x^3
return(result)
}
curve(f, -5,5)
m<- maxNR(f, start = 0)
summary(m)


