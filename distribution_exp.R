https://search.r-project.org/R/refmans/stats/html/Exponential.html
https://de.wikipedia.org/wiki/Exponentialverteilung


Usage
dexp(x, rate = 1, log = FALSE)
pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
qexp(p, rate = 1, lower.tail = TRUE, log.p = FALSE)
rexp(n, rate = 1)

Examples
dexp(1) - exp(-1) #-> 0
dexp(1)
[1] 0.3678794
exp(-1)
[1] 0.3678794

## a fast way to generate *sorted*  U[0,1]  random numbers:
rsunif <- function(n) { n1 <- n+1
   cE <- cumsum(rexp(n1)); cE[seq_len(n)]/cE[n1] }
plot(rsunif(1000), ylim=0:1, pch=".")
abline(0,1/(1000+1), col=adjustcolor(1, 0.5))


Usage: cumsum
cumsum(x)
cumprod(x)
cummax(x)
cummin(x)

cumsum(1:10)
cumprod(1:10)
cummin(c(3:1, 2:0, 4:2))
cummax(c(3:1, 2:0, 4:2))