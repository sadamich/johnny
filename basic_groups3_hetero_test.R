### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)
https://cran.r-project.org/web/packages/car/refman/car.html#leveneTest
educ<- as.factor(EDUC)
leveneTest(LOGSAL ~ educ, xm301)
Levene's Test for Homogeneity of Variance (center = median)
       Df F value   Pr(>F)   
group   9  2.8701 0.002631 **
      464 
leveneTest(lm(LOGSAL ~ educ+logsalbegin, data=xm301))
leveneTest(LOGSAL ~ educ, data=xm301, center=mean)
Levene's Test for Homogeneity of Variance (center = mean)
       Df F value    Pr(>F)    
group   9  3.3626 0.0005258 ***
      464                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
leveneTest(LOGSAL ~ educ, data=xm301, center=mean, trim=0.1)
Levene's Test for Homogeneity of Variance (center = mean: 0.1)
       Df F value    Pr(>F)    
group   9  3.1782 0.0009669 ***
      464                      
---

jobcat<- as.factor(JOBCAT)
leveneTest(LOGSAL ~ jobcat, xm301)
Levene's Test for Homogeneity of Variance (center = median)
       Df F value    Pr(>F)    
group   2  13.114 2.869e-06 ***
      471                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.
