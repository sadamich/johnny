### The maxLik function ###
library(maxLik)
### Source:https://cran.r-project.org/web/packages/maxLik/vignettes/using-maxlik.pdf ###
x<- rnorm(100)
loglik<- function(theta){
mu <- theta[1]
sigma <- theta[2]
sum(dnorm(x, mean=mu, sd = sigma, log=TRUE))
}
m<- maxLik(loglik, start = c(mu=1, sigma=2))
summary(m)