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
micro<- micro[-5]
itemname<- c("ADVMATH1","ADVMATH2","ADVMATH3","CHEMISTRY",
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
          0.069           0.334           0.359           0.411           0.440 
       ADVMATH2       GRADEHIGH     GRINERMICRO       CHEMISTRY 
          0.440           0.471           0.661           0.846 

VSS.scree(micro)

### Seite 460
pca.micro<- principal(micro, 5, rotate="none")
pca.micro$criteria<- NULL
pca.micro
pca.micro_r<- principal(micro, 5)
pca.micro_r$criteria<- NULL
print(pca.micro_r, cut = 0.5, sort=TRUE, digits=2)

### Seite 462
fa.diagram(pca.micro_r, cut=0.5, cex=0.8,rsize=0.5, main="")

### Seite 463
pca.micro2<- principal(micro, 2)
pca.micro2$criteria<- NULL
print(pca.micro2, cut = 0.5, sort=TRUE, digits=2)

fa.diagram(pca.micro2, cut=0.5, cex=0.8,rsize=0.5, main="")


### Seite 468
pca.micro<- principal(micro, 5, scores=TRUE)
head(pca.micro$scores)
micro.scores<- data.frame(pca.micro$scores)
names(micro.scores)<- c("Grade","Math","Character","Intermicro","Math3")


micro.scores<- data.frame(micro.scores, FEMALE)
boxplot(Math ~ FEMALE, data = micro.scores)
describeBy(micro.scores$Math, FEMALE, skew=FALSE)
Descriptive statistics by group 
group: 0
   vars   n mean   sd median   min  max range   se
X1    1 373 0.01 0.98   0.52 -2.06 1.41  3.47 0.05
------------------------------------------------------------ 
group: 1
   vars   n  mean   sd median   min  max range   se
X1    1 236 -0.02 1.04   0.61 -2.04 1.16  3.21 0.07


boxplot(Math3 ~ FEMALE, data = micro.scores)
describeBy(micro.scores$Math3, FEMALE, skew=FALSE)
 Descriptive statistics by group 
group: 0
   vars   n mean   sd median  min max range   se
X1    1 373 0.06 1.17  -0.22 -0.4 9.3  9.71 0.06
------------------------------------------------------------ 
group: 1
   vars   n  mean   sd median   min  max range   se
X1    1 236 -0.09 0.64  -0.22 -0.41 7.37  7.78 0.04

### Seite 470 : Wilcoxon test
wilcox.test(Math ~ FEMALE, data = micro.scores)
  Wilcoxon rank sum test with continuity correction

data:  Math by FEMALE
W = 43888, p-value = 0.9527
alternative hypothesis: true location shift is not equal to 0

wilcox.test(Math3 ~ FEMALE, data = micro.scores)

boxplot(Character ~ FEMALE, data = micro.scores)
wilcox.test(Character ~ FEMALE, data = micro.scores)
  Wilcoxon rank sum test with continuity correction

data:  Character by FEMALE
W = 35621, p-value = 7.266e-05
alternative hypothesis: true location shift is not equal to 0

