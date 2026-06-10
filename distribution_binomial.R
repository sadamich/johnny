https://search.r-project.org/R/refmans/stats/html/Binomial.html

Usage
dbinom(x, size, prob, log = FALSE)
pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
rbinom(n, size, prob)


### CLT B(n,p) -> N(np, np(1-p))                                            ###
### Momenterzeugende Funktion (1-p +pe^t)^n                                 ###
### Charakteristische Funktion (1-p +pe^it)^n                               ###


### Beispiel
### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ##
para <- c(ja = 0.73, nein = 1 - 0.73)
barplot(para)
barplot(para, ylim = c(0,1), width = c(0.5, 0.5), xlim = c(0, 1.5))
barplot(as.matrix(para), ylim = c(0,1), width = 0.2, xlim = c(0, 0.5),
legend = names(para), main = "Glaube an paranormale Phaenomene")
abline(h = 0.76, lty = 2, col = "blue", lwd = 1.5)
beobH <- round(0.73*1008)
prop.test(x = beobH, n = 1008, p = 0.76, alternative = "less", correct = FALSE)
binom.test(x = beobH, n = 1008, p = 0.76, alternative = "less")
