https://cran.r-project.org/web/packages/gmm/refman/gmm.html#charStable
library(gmm)
### Cf) vignettes 3 2 stable distribution                                  ###
https://cran.r-project.org/web/packages/gmm/vignettes/gmm_with_R.pdf
# GMM is like GLS for linear models without endogeneity problems

pm <- 0
theta <- c(1.5,.5,1,0) 
tau <- seq(-3, 3, length.out = 20)
char_fct <- charStable(theta, tau, pm)

plot(char_fct)
θ = (α,β,γ,δ) are the skewness, the scale and the location parameters