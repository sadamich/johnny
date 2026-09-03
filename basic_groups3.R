### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
### Seite 409 
jobcat<- factor(JOBCAT)
levels(jobcat)<- c("Admistration","Custdials","management")
boxplot(LOGSAL~jobcat, xlab="Job categories", ylab="Log Salary")
### Seite 410
mws<- tapply(LOGSAL, jobcat, mean)
sds<- tapply(LOGSAL, jobcat, sd)
rbind(Mean = mws,Sd= sds)
 Admistration   Custdials management
Mean   10.2025355 10.33745249  11.029622
Sd      0.2458633  0.07000588   0.268648

### Seite 411 
bartlett.test(LOGSAL, jobcat) 
  Bartlett test of homogeneity of variances
data:  LOGSAL and jobcat
Bartlett's K-squared = 42.388, df = 2, p-value = 6.245e-10
(keine gleiche Varianzen zwischen 1,2 und 3te Gruppen)F test nicht anwendbar


### Seite 417
boxplot(LOGSAL ~ (jobcat + GENDER), ylab="Log Salary")
mw<- tapply(LOGSAL, list(jobcat, GENDER), mean)
round(mw,digits=2)
 0     1
1 10.10 10.33
2    NA 10.34
3 10.75 11.07

par(mfrow= c(1,2))
interaction.plot(GENDER, jobcat,LOGSAL, ylab = "Mean (Log salary)")
interaction.plot(jobcat, GENDER, LOGSAL, ylab ="Mean (LOG salary)")

### Seite 419 Varianzanalyse
eq1<- lm(LOGSAL ~ jobcat *GENDER)
anova(eq1)
eq2<- lm(LOGSAL ~ jobcat + GENDER)
anova(eq1,eq2)


eq0<- lm(LOGSAL ~1)
anova(eq1,eq0)
eq01<- lm(LOGSAL ~ jobcat)
anova(eq1, eq01)
eq02<- lm(LOGSAL ~ GENDER)
anova(eq1,eq02)
