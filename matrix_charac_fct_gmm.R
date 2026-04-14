https://cran.r-project.org/web/packages/gmm/refman/gmm.html#charStable

# GMM is like GLS for linear models without endogeneity problems
library(gmm)
pm <- 0
theta <- c(1.5,.5,1,0) 
tau <- seq(-3, 3, length.out = 20)
char_fct <- charStable(theta, tau, pm)
char_fct
plot(char_fct, type="l")

