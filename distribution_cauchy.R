https://search.r-project.org/R/refmans/stats/html/Cauchy.html
https://de.wikipedia.org/wiki/Cauchy-Verteilung
Usage
dcauchy(x, location = 0, scale = 1, log = FALSE)
pcauchy(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
qcauchy(p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
rcauchy(n, location = 0, scale = 1)


x<- dcauchy(-1:4)
hist(x)

set.seed(33)
y<- rcauchy(100, 0,1)
hist(y)