https://search.r-project.org/R/refmans/stats/html/Uniform.html

Usage
dunif(x, min = 0, max = 1, log = FALSE)
punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
runif(n, min = 0, max = 1)

Zum Nachdenken der Anwendbarkeit der Gleichverteilung 
### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### S. 229 - 243                                                            ###
kstand <- read.table("kstand.dat", header = TRUE)
attach(kstand)
WOCHENTAG[1:50]
table(WOCHENTAG)
x<- c(60,45,30,51,96,9,9)
hist(x)
### the histgram does not represent a uniform distribution.                 ###

table(WOCHENTAG)/length(WOCHENTAG)
prop.table(table(WOCHENTAG))
100*prop.table(table(WOCHENTAG))
wtag <- factor(WOCHENTAG, levels = c("MO", "DI", "MI", "DO", "FR", "SA", "SO"))
detach(kstand)
absH <- table(wtag)
relH <- prop.table(table(wtag))
proz <- 100*relH
relH <- round(relH, digits = 2)
proz <- round(proz, digits = 0)
cbind(absH, relH, proz)
barplot(absH)
pie(absH)
lab <- paste0(names(absH), "\n(", proz, "%)")
pie(absH, labels = lab, clockwise = TRUE)

erwH <- rep(300/7, 7)
residum <- absH - erwH
erwH <- round(erwH, digits = 1)
residum < round(residum, digits = 1)
cbind(absH, erwH, residum)
chisq.test(table(wtag))

whaelfte <- ifelse(wtag %in% c("SA", "SO", "MO", "DI"), "SA-DI", "MI-FR")
table(whaelfte)
chisq.test(table(whaelfte))