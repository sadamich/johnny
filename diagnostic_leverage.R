### https://cran.r-project.org/web/packages/gslnls/refman/gslnls.html#hatvalues.gsl_nls ##
library(gslnls)
### data                                                                     ###
set.seed(1)
n <- 25
xy <- data.frame(
 x = (1:n) / n,
 y = 2.5 * exp(-1.5 * (1:n) / n) + rnorm(n, sd = 0.1)
)
attach(xy)
plot(x,y)
### model                                                                    ###
obj <- gsl_nls(fn = y ~ A * exp(-lam * x), data = xy, start = c(A = 1, lam = 1))

z<- hatvalues(obj)
plot(z)
