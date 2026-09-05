### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
library(psych)
xm608micro<- read.csv("xm608micro.csv", header =TRUE)
str(xm608micro)
attach(xm608micro)
### Seite 457 
micro<- na.omit(xm608micro[ , 2:11])
itemname<- c("ADVMATH1","ADVMATH2","ADVMATH3","CHEMISTRY","FEMALE",
             "FRESHMAN","GRADEHIGH","GRADELOW","GRADFINTERMICRO",
             "GRINERMICRO")
colnames(micro)<- itemname
### Seite 458 
library(REdaS)
bart_spher(micro)
  Bartlett's Test of Sphericity
Call: bart_spher(x = micro)
     X2 = 2357.113
     df = 45
p-value < 2.22e-16
kmos<- KMOS(micro)
print(kmos, stats ="KMO")
Kaiser-Meyer-Olkin Statistic
Call: KMOS(x = micro)
KMO-Criterion: 0.4371419
### Seite 459 
print(kmos,stats ="MSA", sort = TRUE, digits=3)
Kaiser-Meyer-Olkin Statistics
Call: KMOS(x = micro)
Measures of Sampling Adequacy (MSA):
       ADVMATH3 GRADFINTERMICRO        FRESHMAN        GRADELOW        ADVMATH1 
          0.078           0.334           0.378           0.412           0.440 
       ADVMATH2       GRADEHIGH     GRINERMICRO          FEMALE       CHEMISTRY 
          0.441           0.473           0.668           0.744           0.792 

VSS.scree(micro)

### Seite 460
pca.micro<- principal(micro, 5, rotate="none")
pca.micro$criteria<- NULL
pca.micro