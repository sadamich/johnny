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


### Seite 509
g_m<- table(GENDER,MINORITY)
addmargins(g_m)
 MINORITY
GENDER   0   1 Sum
   0   176  40 216
   1   194  64 258
   Sum 370 104 474
g_m
  MINORITY
GENDER   0   1
     0 176  40
     1 194  64
chisq.test(g_m)
  Pearson's Chi-squared test with Yates' continuity correction
data:  g_m
X-squared = 2.3592, df = 1, p-value = 0.1245 (H0 ist nicht verworfen)

table(GENDER)
GENDER
  0   1 
216 258
chit_g<- chisq.test(table(GENDER))
 Chi-squared test for given probabilities
data:  table(GENDER)
X-squared = 3.7215, df = 1, p-value = 0.05372

chit_m<- chisq.test(table(MINORITY))
 Chi-squared test for given probabilities
data:  table(MINORITY)
X-squared = 149.27, df = 1, p-value < 2.2e-16

erwart<- chit_g$expected
erwart
0   1 
237 237 

### Seite 518
jobcat<- factor(JOBCAT)
levels(jobcat)<- c("Admistration","Custdials","management")
gendertab<- table(jobcat, GENDER)
gendertab
  GENDER
jobcat           0   1
  Admistration 206 157
  Custdials      0  27
  management    10  74

gender_dfr<- as.data.frame(gendertab)
eq_logit<- glm(Freq ~ GENDER, family= poisson, data=gender_dfr)
summary(eq_logit)
Residual deviance: 497.81  on 4  degrees of freedom
1 - pchisq(497.81,3)
[1] 0
eq_logit2<- glm(Freq ~ GENDER+jobcat, family= poisson, data=gendertab)
summary(eq_logit2)
Residual deviance:  95.463  on 2  degrees of freedom
1 - pchisq(95.463  ,2)
obs<- eq_logit2$y
erw<- fitted(eq_logit2)
resLR<- residuals(eq_logit2)
resx2<- (obs - erw)/sqrt(erw)
cbind(obs,erw,resx2,resLR)
PearsonX2<- sum(resx2^2)
PearsonX2

eq_logit3<- glm(Freq ~ GENDER*jobcat, family= poisson, data=gendertab)
summary(eq_logit3)
Residual deviance: 4.1225e-10  on 0  degrees of freedom
1 - pchisq(4.1225e-10,1)
obs<- eq_logit2$y
erw<- fitted(eq_logit3)
resLR<- residuals(eq_logit3)
resx2<- (obs - erw)/sqrt(erw)
cbind(obs,erw,resx2,resLR)
  obs          erw         resx2 resLR
1 206 2.060000e+02 -2.772328e-14     0
2   0 2.061154e-10 -1.435672e-05     0
3  10 1.000000e+01 -3.595093e-14     0
4 157 1.570000e+02 -1.814639e-14     0
5  27 2.700000e+01 -1.846044e-14     0
6  74 7.400000e+01 -1.156385e-14     0
> 
