https://cran.r-project.org/web/packages/dsge/
https://cran.r-project.org/web/packages/dsge/refman/dsge.html#bayes_dsge
install.packages("dsge")
library(dsge)
https://cran.r-project.org/web/packages/dsge/vignettes/introduction.html

nk <- dsge_model(
  obs(p   ~ beta * lead(p) + kappa * x),
  unobs(x ~ lead(x) - (r - lead(p) - g)),
  obs(r   ~ psi * p + u),
  state(u ~ rhou * u),
  state(g ~ rhog * g),
  fixed = list(beta = 0.96),
  start = list(kappa = 0.1, psi = 1.5, rhou = 0.7, rhog = 0.9)
)

print(nk)

Linear DSGE Model
  Observed controls:   p, r 
  Unobserved controls: x 
  Exogenous states:    u, g 
  Parameters:          beta, kappa, psi, rhou, rhog 
  Fixed:               beta = 0.96 
  Equations:            5 

Equations:
   p ~ beta * lead(p) + kappa * x   [observed] 
   x ~ lead(x) - (r - lead(p) - g)   [unobserved] 
   r ~ psi * p + u   [observed] 
   u ~ rhou * u   [state] 
   g ~ rhog * g   [state] 


# Solve at true parameters to get state-space form
true_params <- c(beta = 0.96, kappa = 0.085, psi = 1.94,
                 rhou = 0.70, rhog = 0.95)
true_sd <- c(u = 2.3, g = 0.57)
sol <- solve_dsge(nk, params = true_params, shock_sd = true_sd)

# Simulate data
set.seed(123)
n <- 200
states <- matrix(0, n, 2)
colnames(states) <- c("u", "g")
for (t in 2:n) {
  states[t, "u"] <- 0.70 * states[t - 1, "u"] + 2.3 * rnorm(1)
  states[t, "g"] <- 0.95 * states[t - 1, "g"] + 0.57 * rnorm(1)
}
Z <- sol$D %*% sol$G
obs_data <- states %*% t(Z)
colnames(obs_data) <- c("p", "r")
nk_dat <- as.data.frame(obs_data)

# Estimate
nk_fit <- estimate(nk, data = nk_dat,
                   start = list(kappa = 0.1, psi = 1.5,
                                rhou = 0.7, rhog = 0.9),
                   control = list(maxit = 500))
summary(nk_fit)

DSGE Model -- Summary
============================================================ 

  Log-likelihood:  -594.45234 
  AIC:             1200.9 
  BIC:             1220.69 
  Observations:    200 
  Convergence:     Yes 

Structural parameters:
                      Estimate    Std. Err.        z    P>|z|   [95% CI]
------------------------------------------------------------------------------ 
  beta                0.960000      (fixed)                  
  kappa               0.175999     0.065131     2.70   0.0069   [  0.0483,   0.3037]
  psi                 1.888534     0.438666     4.31   0.0000   [  1.0287,   2.7483]
  rhou                0.605425     0.055990    10.81   0.0000   [  0.4957,   0.7152]
  rhog                0.866206     0.034497    25.11   0.0000   [  0.7986,   0.9338]

Shock standard deviations:
  sd(e.u)             2.086544     0.424927     4.91   0.0000   [  1.2537,   2.9194]
  sd(e.g)             0.627729     0.123601     5.08   0.0000   [  0.3855,   0.8700]


Stability:
  Saddle-path stable:  TRUE 
  Eigenvalue moduli:   0.8662, 0.6054 

 nk_irfs <- irf(nk_fit, periods = 20)
plot(nk_irfs)


nk_fc <- forecast(nk_fit, horizon = 12)
plot(nk_fc)

