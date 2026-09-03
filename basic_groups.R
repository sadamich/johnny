### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###

xm101<- read.csv("xm101.csv", header=TRUE)
str(xm101)
attach(xm101)
detach(xm101)
### Seite 403
boxplot(FGPA ~ FEM, ylab ="FGPA")
### Seite 404
mws<- tapply(FGPA, FEM, mean)
sds<- tapply(FGPA, FEM, sd)
rbind(mean= mws,sd=sds)
      0         1
mean 2.7282386 2.8948305
sd   0.4412611 0.4719426
### Seite 405
t.test(FGPA ~ FEM)
 Welch Two Sample t-test

data:  FGPA by FEM
t = -4.3513, df = 475.05, p-value = 1.658e-05
alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
95 percent confidence interval:
 -0.24182191 -0.09136189
sample estimates:
mean in group 0 mean in group 1 
       2.728239        2.894831 

### Seite 411
bartlett.test(FGPA, FEM)
        Bartlett test of homogeneity of variances

data:  FGPA and FEM
Bartlett's K-squared = 1.3115, df = 1, p-value = 0.2521

### F test: Exercise 3 10 (d) (p.182) 
eq<- aov(FGPA ~ FEM)
summary(eq)
 Df Sum Sq Mean Sq F value   Pr(>F)    
FEM           1   4.01   4.012   19.52 1.18e-05 ***
Residuals   607 124.77   0.206                     
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 

