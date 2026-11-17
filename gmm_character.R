https://cran.r-project.org/web/packages/gmm/index.html
https://cran.r-project.org/web/packages/gmm/refman/gmm.html#charStable
https://de.wikipedia.org/wiki/Charakteristische_Funktion_(Stochastik)

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


https://edspace.american.edu/jpnolan/stable/
https://edspace.american.edu/jpnolan/wp-content/uploads/sites/1720/2020/09/Chap1.pdf
https://edspace.american.edu/jpnolan/wp-content/uploads/sites/1720/2024/06/Book2Chapter1.pdf

