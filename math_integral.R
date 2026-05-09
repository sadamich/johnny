https://search.r-project.org/R/refmans/stats/html/integrate.html

Examples
integrate(dnorm, -1.96, 1.96)
[1] 0.9500042 with absolute error < 1e-11
integrate(dnorm, -Inf, Inf)
[1] 1 with absolute error < 9.4e-05

## a slowly-convergent integral
integrand <- function(x) {1/((x+1)*sqrt(x))}
integrate(integrand, lower = 0, upper = Inf)
[1] 3.141593 with absolute error < 2.7e-05

integrand<- function(x){2*x}
integrate(integrand, lower=0, upper = 3)
[1] 9 with absolute error < 1e-13


### Integration and normal distribution                                   ###
integrand <- function(x) {1/sqrt(2*pi)*exp(-0.5*x^2)}
## don't do this if you really want the integral from 0 to Inf
integrate(integrand, lower = 0, upper = 10)
integrate(integrand, lower = 0, upper = 100000)
integrate(integrand, lower = 0, upper = 1000000, stop.on.error = FALSE)

## some functions do not handle vector input properly
f <- function(x) 2.0
try(integrate(f, 0, 1))
integrate(Vectorize(f), 0, 1)  ## correct
integrate(function(x) rep(2.0, length(x)), 0, 1)  ## correct

## integrate can fail if misused
integrate(dnorm, 0, 2)
integrate(dnorm, 0, 20)
integrate(dnorm, 0, 200)
integrate(dnorm, 0, 2000)
integrate(dnorm, 0, 20000) ## "fails" on many systems -- "wrongly" giving '0' 
integrate(dnorm, 0, Inf)   ## works

integrate(dnorm, 0:1, 20) #-> error!
## "silently" gave  integrate(dnorm, 0, 20)  in earlier versions of R

### Chisq distribution and expected values                                 ###
E[X] = interate(x f(x) dx)
f(x) = c X^(n/2 -1) exp(-1/2)

(u(x) + v(x)) ' = u'(x) v(x) + u(x)v'(x)

integrate((u(x)v'(x)) dx = u(x) v(x) - interate(u'(x)v(x)) dx

f(t) = lambda(t) s(t)

s(t) = exp( - integrate (lambda(s) ds)

