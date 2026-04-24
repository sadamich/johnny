https://cran.r-project.org/web/packages/psych/refman/psych.html#cortest.bartlett

library(psych)

set.seed(42)   
x <- matrix(rnorm(1000),ncol=10)
r <- cor(x)
cortest.bartlett(r)      #random data don't differ from an identity matrix
$chisq
[1] 33.78894

$p.value
[1] 0.8897656

$df
[1] 45
#data(bfi)
cortest.bartlett(bfi[1:200,1:10])    #not an identity matrix
R was not square, finding R from data
$chisq
[1] 457.457

$p.value
[1] 1.136106e-69

$df
[1] 45

f3 <- fa(Thurstone,3)
f3r <- f3$resid
cortest.bartlett(f3r,n=213,diag=FALSE)  #incorrect
$chisq
[1] 2203.356

$p.value
[1] 0

$df
[1] 36
cortest.bartlett(f3r,n=213,diag=TRUE)  #correct (by default)

$chisq
[1] 0.256498

$p.value
[1] 1

$df
[1] 36