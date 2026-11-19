https://cran.r-project.org/web/packages/dsge/
https://cran.r-project.org/web/packages/dsge/refman/dsge.html#bayes_dsge
install.packages("dsge")
library(dsge)
https://cran.r-project.org/web/packages/dsge/vignettes/introduction.html
### The specification of the model
set.seed(42)
true_rho <- 0.8
true_sd <- 1.0
n <- 200
z <- numeric(n)
for (i in 2:n) z[i] <- true_rho * z[i - 1] + true_sd * rnorm(1)
dat <- data.frame(y = z)

m <- dsge_model(
  obs(y ~ z),
  state(z ~ rho * z),
  start = list(rho = 0.5)
)
print(m)
Linear DSGE Model
  Observed controls:   y 
  Unobserved controls:  
  Exogenous states:    z 
  Parameters:          rho 
  Equations:            2 
Equations:
   y ~ z   [observed] 
   z ~ rho * z   [state] 

### The solving of unknown parameter
sol <- solve_dsge(m, params = c(rho = 0.8))
print(sol)
DSGE Solution
  Stable:  TRUE 
  Stable eigenvalues:  1 / 1 

Policy matrix (G):
  z
y 1

Transition matrix (H):
    z
z 0.8

fit <- estimate(m, data = dat)
summary(fit)
DSGE Model -- Summary
============================================================ 

  Log-likelihood:  -278.60475 
  AIC:             561.21 
  BIC:             567.806 
  Observations:    200 
  Convergence:     Yes 

Structural parameters:
                      Estimate    Std. Err.        z    P>|z|   [95% CI]
------------------------------------------------------------------------------ 
  rho                 0.787161     0.043366    18.15   0.0000   [  0.7022,   0.8722]
Shock standard deviations:
  sd(e.z)             0.972066     0.048605    20.00   0.0000   [  0.8768,   1.0673]

Stability:
  Saddle-path stable:  TRUE 
  Eigenvalue moduli:   0.7872 


# Policy matrix (maps states to controls)
policy_matrix(fit, se = FALSE)
   z
 y 1

transition_matrix(fit, se = FALSE)
          z
z 0.7871615

stability(fit)
Stability Check
  Saddle-path stable:  TRUE 
  Stable eigenvalues:  1  /  1 
  Required stable:     1 

Eigenvalues:
  0.787161  |lambda| = 0.787161  [stable]

irfs <- irf(fit, periods = 20, se = FALSE)
plot(irfs)

fc <- forecast(fit, horizon = 12)
plot(fc)



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