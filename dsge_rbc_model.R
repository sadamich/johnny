https://cran.r-project.org/web/packages/dsge/
https://cran.r-project.org/web/packages/dsge/refman/dsge.html#bayes_dsge
install.packages("dsge")
library(dsge)
https://cran.r-project.org/web/packages/dsge/vignettes/introduction.html
rbc <- dsgenl_model(
  "1/C = beta / C(+1) * (alpha * exp(Z) * K^(alpha-1) + 1 - delta)",
  "K(+1) = exp(Z) * K^alpha - C + (1 - delta) * K",
  "Z(+1) = rho * Z",
  observed = "C",
  endo_state = "K",
  exo_state = "Z",
  fixed = list(alpha = 0.33, beta = 0.99, delta = 0.025),
  start = list(rho = 0.9),
  ss_guess = c(C = 2, K = 30, Z = 0)
)
print(rbc)

Nonlinear DSGE Model
  Equations:     3 
  Controls:      1 ( 1 observed, 0 unobserved )
  States:        2 ( 1 exogenous, 1 endogenous )
  Parameters:    4 ( 1 free, 3 fixed )

  [1] 1/C = beta / C(+1) * (alpha * exp(Z) * K^(alpha-1) + 1 - delta)
  [2] Z(+1) = rho * Z
  [3] K(+1) = exp(Z) * K^alpha - C + (1 - delta) * K

  Fixed:  alpha = 0.33, beta = 0.99, delta = 0.025 


params <- c(alpha = 0.33, beta = 0.99, delta = 0.025, rho = 0.9)
ss <- steady_state(rbc, params = params)
print(ss)
Deterministic Steady State
  Converged: TRUE  ( 5  iterations)
  Max residual: 0 

  Values:
     C = 2.306617 
     Z = 0 
     K = 28.34842 


sol <- solve_dsge(rbc, params = params, shock_sd = c(Z = 0.01))
print(sol)
DSGE Solution
  Stable:  TRUE 
  Stable eigenvalues:  2 / 2 

Policy matrix (G):
         Z        K
C 0.451385 0.048867

Transition matrix (H):
         Z        K
Z 0.900000 0.000000
K 2.563943 0.961234

stability(sol)
DSGE Stability Check
  Saddle-path stable:  TRUE 
  Stable eigenvalues:  2  /  2 
  Required stable:     2 

Eigenvalues:
  0.961234  |lambda| = 0.961234  [stable]
  0.9  |lambda| = 0.900000  [stable]

rbc_irfs <- irf(sol, periods = 40, se = FALSE)
plot(rbc_irfs)