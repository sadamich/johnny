### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
library(psych)

xm608macro<- read.csv("xm608macro.csv", header =TRUE)
str(xm608macro)
attach(xm608macro)

macro<- na.omit(xm608macro[ , 2:11])
itemname<- c("ADVMATH1","ADVMATH2","ADVMATH3","CHEMISTRY","FEMALE",
             "FRESHMAN","GRADEHIGH","GRADELOW","GRADFINTERMACRO",
             "GRINERMACRO")
colnames(macro)<- itemname
### Seite 458 
library(REdaS)
bart_spher(macro)