https://cran.r-project.org/web/packages/dsge/refman/dsge.html#bayes_dsge
install.packages("dsge")
library(dsge)


m <- dsge_model(
  obs(y ~ z),
  state(z ~ rho * z),
  start = list(rho = 0.5)
)

set.seed(42)
z <- numeric(200); for (i in 2:200) z[i] <- 0.8 * z[i-1] + rnorm(1)
dat <- data.frame(y = z)

fit <- bayes_dsge(m, data = dat,
                  priors = list(rho = prior("beta", shape1 = 2, shape2 = 2)),
                  chains = 2, iter = 2000, seed = 1)
summary(fit)
Bayesian DSGE estimation (RWMH, linear)
  Chains: 2  Iterations: 2000  Warmup: 1000 
  Observations: 200 
  Acceptance rates: 0.297, 0.274 

Posterior summary:
         mean     sd   2.5%    50%  97.5%   ess   rhat   mcse
rho    0.7797 0.0397 0.7000 0.7802 0.8510 284.0 1.0020 0.0024
sd_e.z 0.9837 0.0507 0.8819 0.9845 1.0808 171.7 1.0033 0.0039


### der natuerliche Zins  seite 6 und 10                                   ###
https://www.bundesbank.de/resource/blob/693452/be1c527c9b862139c54f849c16819b51/472B63F073F071307366337C94F8C870/2017-10-natuerlicher-zins-data.pdf