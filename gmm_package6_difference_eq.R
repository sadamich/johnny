https://cran.r-project.org/web/packages/gmm/index.html
### 3 7 stochstic difference equation Approximation discret
g6 <- function(theta, x) {
t <- length(x)
et1 <- diff(x)- theta[1]- theta[2] * x[-t]
ht <- et1^2- theta[3] * x[-t]^(2 * theta[4])
g <- cbind(et1, et1 * x[-t], ht, ht * x[-t])
return(g)
}

rf <- Finance[,"rf"]
rf <- ((1 + rf/100)^(365)- 1) * 100
dr <- diff(rf)
res_0 <- lm(dr ~ rf[-length(rf)])
tet0 <- c(res_0$coef, var(residuals(res_0)), 0)
names(tet0) <- c("alpha", "beta", "sigma^2", "gamma")
res_rf <- gmm(g6, rf, tet0, control = list(maxit = 1000, reltol = 1e-10))
coef(res_rf)