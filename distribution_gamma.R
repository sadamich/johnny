https://search.r-project.org/R/refmans/stats/html/GammaDist.html

https://de.wikipedia.org/wiki/Gammaverteilung

dgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE)
pgamma(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
       log.p = FALSE)
qgamma(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
       log.p = FALSE)
rgamma(n, shape, rate = 1, scale = 1/rate)


Examples
-log(dgamma(1:4, shape = 1))
p <- (1:9)/10
plot(p)
hist(p)

pgamma(qgamma(p, shape = 2), shape = 2)
plot(pgamma(qgamma(p, shape = 2), shape = 2))
1 - 1/exp(qgamma(p, shape = 1))

# even for shape = 0.001 about half the mass is on numbers
# that cannot be represented accurately (and most of those as zero)
pgamma(.Machine$double.xmin, 0.001)
pgamma(5e-324, 0.001)  # on most machines 5e-324 is the smallest
                       # representable non-zero number
table(rgamma(1e4, 0.001) == 0)/1e4