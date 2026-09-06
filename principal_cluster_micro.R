### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###

library(cluster)
xm608micro<- read.csv("xm608micro.csv", header =TRUE)
str(xm608micro)
attach(xm608micro)
### Seite 457 
micro<- na.omit(xm608micro[ , 2:12])
varclust<- agnes(t(micro), stand=TRUE)
varclust$ac
[1] 0.6097787

pltree(varclust, main ="Variable Cluster", xlab ="Student data", sub="")