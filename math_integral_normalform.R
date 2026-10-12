integrand <- function(x) {exp(-0.5*x^2)}
integrate(integrand, lower = -10, upper = 10)
2.506628 with absolute error < 0.00019

integrate(integrand, lower = -100, upper = 100)
2.506628 with absolute error < 8.1e-07

integrate(integrand, lower = -Inf, upper = Inf)
2.506628 with absolute error < 0.00023

curve(integrand, -10, 10)