### Quelle:                                                                 ###
### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Kapitel 11 (S. 466-470)                                                 ###
library("psych")
library("psych")
smarkt <- read.table("smarkt.dat", header = TRUE)
smd <- na.omit(smarkt[, 6:25])
pca.smd<- principal(smd,5, scores =TRUE)
head(pca.smd$scores)
          RC1         RC2        RC5        RC3         RC4
25  0.8384468 -0.99689649 -0.4504262  0.2180845 -0.03377116
57 -0.3408142 -1.66215925  0.2676716 -0.6371299  0.36852006
66 -1.0042197 -0.08216886  1.4708867 -0.7463414 -0.19062172
86  0.4698299 -0.98809774 -0.6701671  0.4710615  0.97684923
87  0.9250770  0.30530920  1.0300059 -0.2480888 -1.34447243
88  0.9250770  0.30530920  1.0300059 -0.2480888 -1.34447243

smd.scores<- data.frame(pca.smd$scores)
names(smd.scores)<- c("Qual","Serv","Preis-Leis","Auto","Bequem")
SEX<- smarkt$sex
idx<- as.numeric(rownames(smd.scores))
SEX<- SEX[idx]
smd.scores<- data.frame(smd.scores, SEX)
head(smd.scores)
   Qual        Serv Preis.Leis       Auto      Bequem         SEX
25  0.8384468 -0.99689649 -0.4504262  0.2180845 -0.03377116    weiblich
57 -0.3408142 -1.66215925  0.2676716 -0.6371299  0.36852006    maennlich
66 -1.0042197 -0.08216886  1.4708867 -0.7463414 -0.19062172    weiblich
86  0.4698299 -0.98809774 -0.6701671  0.4710615  0.97684923    weiblich
87  0.9250770  0.30530920  1.0300059 -0.2480888 -1.34447243    weiblich
88  0.9250770  0.30530920  1.0300059 -0.2480888 -1.34447243    weiblich

boxplot(Qual ~ SEX, data= smd.scores)
str(SEX)
Geschlecht<- factor(SEX, labels=c("Maennlich", "Weiblich"))
boxplot(Qual ~ Geschlecht, data= smd.scores, ylab="Qualitaet")

describeBy(smd.scores$Qual, SEX, skew=FALSE)
y group 
group: maennlich
   vars   n  mean   sd median   min  max range  se
X1    1 120 -0.19 1.06   0.03 -3.61 1.75  5.37 0.1
------------------------------------------------------------ 
group: weiblich
   vars   n mean   sd median   min  max range   se
X1    1 371 0.06 0.97   0.24 -3.73 2.08  5.81 0.05

wilcox.test(Qual ~ SEX, data=smd.scores)

        Wilcoxon rank sum test with continuity correction

data:  Qual by SEX
W = 18974, p-value = 0.01501
alternative hypothesis: true location shift is not equal to 0

