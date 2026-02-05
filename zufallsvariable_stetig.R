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
summary(vwghdauer)


