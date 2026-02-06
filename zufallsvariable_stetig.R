### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
vwgh<- read.csv2("vwgh.csv", header =TRUE)
str(vwgh)
attach(vwgh)
hist(dauer1)
hist(dauer2)
hist(dauer3)
vwghdauer <- with(vwgh, dauer3[dauer3 != -9999])
hist(vwghdauer)
### Masszahlen zur Beschreibung der Verteilung, S.309 ###
summary(vwghdauer)
var(vwghdauer)
sd(vwghdauer)
### Min, 1.Quartil, Median, 3.Quartil. Max ###
fivenum(vwghdauer)
### Boxplot                                ###
boxplot(vwghdauer, ylab = "Dauer in Tagen")
### T. Test                         S. 329 ###
t.test(vwghdauer, mu = 456, alternative ="greater")
### Konfidenzintervall              S. 331 ###
t.test(vwghdauer)$conf.int

