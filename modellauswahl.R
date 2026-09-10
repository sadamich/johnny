### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Modellauswahl : Seite 520
cannabis <- read.table("cannabis.dat" ,header= TRUE)
attach(cannabis)
str(cannabis)
detach(cannabis)
canntab <- table(risiko, alter)
canntab

  alter
risiko         15-18 19-21 22-24
  eher hoch      154   100    50
  eher niedrig    89    63    32

can.dfr<- as.data.frame(canntab)
### Modell alter
modage<- glm(Freq ~ alter, family = poisson, data = can.dfr)
summary(modage)
1 - pchisq(30.056, 3)
obs <- modage$y
erw <- fitted(modage)
resLR <- residuals(modage)
resx2 <- (obs - erw)/sqrt(erw)
cbind(obs, erw, resx2, resLR)

### Modell alter + risiko
eq2<- glm(Freq ~ alter+risiko, family = poisson, data = can.dfr)
summary(eq2)
Residual deviance:   0.24334  on 2  degrees of freedom: G^2 Werte
1 - pchisq(0.24334, 2)
[1] 0.8854405
obs <- eq2$y
erw <- fitted(eq2)
resLR <- residuals(eq2)
resx2 <- (obs - erw)/sqrt(erw)
cbind(obs, erw, resx2, resLR)


eq3<- glm(Freq ~ alter*risiko, family = poisson, data = can.dfr)
summary(eq3)
Residual deviance:   0.24334  on 2  degrees of freedom: G^2 Werte
1 - pchisq(3.3307e-14 , 1)
[1] 0.9999999
obs <- eq3$y
erw <- fitted(eq3)
resLR <- residuals(eq3)
resx2 <- (obs - erw)/sqrt(erw)
cbind(obs, erw, resx2, resLR)
 obs erw         resx2 resLR
1 154 154 -4.580573e-15     0
2  89  89 -6.025390e-15     0
3 100 100  4.263256e-15     0
4  63  63  8.951997e-16     0
5  50  50  4.019437e-15     0
6  32  32  2.512148e-15     0

eq4<- glm(Freq ~ risiko, family = poisson, data = can.dfr)
summary(eq4)
Residual deviance:  83.633  on 4  degrees of freedom
1 - pchisq( 83.633, 4)
[1] 0
obs <- eq4$y
erw <- fitted(eq4)
resLR <- residuals(eq4)
resx2 <- (obs - erw)/sqrt(erw)
cbind(obs, erw, resx2, resLR)
 obs       erw      resx2      resLR
1 154 101.33333  5.2319028  4.8555248
2  89  61.33333  3.5327146  3.3073196
3 100 101.33333 -0.1324532 -0.1327453
4  63  61.33333  0.2128141  0.2118611
5  50 101.33333 -5.0994496 -5.6592779
6  32  61.33333 -3.7455287 -4.1266285

eq5<- glm(Freq ~ 1, family = poisson, data = can.dfr)
summary(eq5)
Residual deviance: 113.45  on 5  degrees of freedom
1 - pchisq( 113.45, 5)
[1] 0
obs <- eq5$y
erw <- fitted(eq5)
resLR <- residuals(eq5)
resx2 <- (obs - erw)/sqrt(erw)
cbind(obs, erw, resx2, resLR)
 obs      erw      resx2      resLR
1 154 81.33333  8.0575119  7.1619020
2  89 81.33333  0.8501045  0.8372477
3 100 81.33333  2.0698196  1.9973774
4  63 81.33333 -2.0328585 -2.1174499
5  50 81.33333 -3.4743400 -3.7434441
6  32 81.33333 -5.4702374 -6.2422900

