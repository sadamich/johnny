https://search.r-project.org/R/refmans/stats/html/Weibull.html


Usage
dweibull(x, shape, scale = 1, log = FALSE)
pweibull(q, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)
qweibull(p, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)
rweibull(n, shape, scale = 1)


x <- c(0, rlnorm(50))
hist(x)
### Weibull = Exponential 
all.equal(dweibull(x, shape = 1), dexp(x))
curve(dweibull(x, shape = 1))
curve(dexp(x))
### Weibull = Exponential : schal
all.equal(pweibull(x, shape = 1, scale = pi), pexp(x, rate = 1/pi))
curve(pweibull(x, shape = 1, scale = pi))
curve( pexp(x, rate = 1/pi))

## Cumulative hazard H():
all.equal(pweibull(x, 2.5, pi, lower.tail = FALSE, log.p = TRUE),
          -(x/pi)^2.5, tolerance = 1e-15)
all.equal(qweibull(x/11, shape = 1, scale = pi), qexp(x/11, rate = 1/pi))

curve(pweibull(x, 2.5, pi, lower.tail = FALSE, log.p = TRUE))