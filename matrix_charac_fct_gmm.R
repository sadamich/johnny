https://cran.r-project.org/web/packages/gmm/refman/gmm.html#charStable

# GMM is like GLS for linear models without endogeneity problems
library(gmm)
pm: The type of parametization. It takes the values 0 or 1.
pm <- 0
theta:Vector of parameters of the stable distribution
theta <- c(1.5,.5,1,0) 
tau:A vector of numbers at which the function is evaluated
tau <- seq(-3, 3, length.out = 20)
char_fct <- charStable(theta, tau, pm)
char_fct
plot(char_fct, type="l")


https://de.wikipedia.org/wiki/Charakteristische_Funktion_(Stochastik)

