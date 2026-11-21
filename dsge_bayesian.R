https://cran.r-project.org/web/packages/dsge/
https://cran.r-project.org/web/packages/dsge/refman/dsge.html#bayes_dsge
install.packages("dsge")
library(dsge)
https://cran.r-project.org/web/packages/dsge/vignettes/introduction.html

my_priors <- list(
  beta  = prior("beta", shape1 = 95, shape2 = 5),
  kappa = prior("beta", shape1 = 30, shape2 = 70),
  psi   = prior("gamma", shape = 20, rate = 13.3),
  rhou  = prior("beta", shape1 = 70, shape2 = 20),
  rhog  = prior("beta", shape1 = 70, shape2 = 20)
  # shock SDs default to inv_gamma(0.01, 0.01)
)

fit_bayes <- bayes_dsge(nk, data = nk_dat, priors = my_priors,
                        chains = 2, iter = 10000, warmup = 5000,
                        seed = 42)

summary(fit_bayes)