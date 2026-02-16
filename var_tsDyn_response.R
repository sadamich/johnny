### Generalized Impulse response Function (GIRF)                             ###
library(tsDyn)
## simulate a SETAR for the example. Higher regime more persistent 
### (AR coef 0.5 instead of 0.2)
set <- setar.sim(B = c(0.5, 0.2, 0.5, 0.5), lag = 1, Thresh = 0.5, n = 500)
set_estim <- setar(set, m = 1)

## regime-specific IRF:
plot(irf(set_estim, regime = "L", boot = FALSE))
plot(irf(set_estim, regime = "H", boot = FALSE))

## GIRF
girf_out <- GIRF(set_estim, n.hist = 10, n.shock = 10)
 # smaller number for example only

## the GIRF shows a very fast convergence (the shock at n.ahead = 4 is 
  already very close to 0)
plot(girf_out, n.ahead = 1:4)
## investigate a few specific GIRFS:
plot(girf_out, plot_type = "line", n_simu  = 1:5)

### Impulse response function                                                ###
data(barry)
str(barry)
## For VAR
mod_var <- lineVar(barry, lag = 2)
irf(mod_var, impulse = "dolcan", response = c("dolcan", "cpiUSA", "cpiCAN"), boot =
FALSE)

## For VECM
mod_VECM <- VECM(barry, lag = 2, estim="ML", r=2)
irf(mod_VECM, impulse = "dolcan", response = c("dolcan", "cpiUSA", "cpiCAN"), boot =
FALSE)