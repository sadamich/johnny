### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 21 Interest and Bond rates (p.360)                           ###
xm511<- read.csv("xm511.csv",header =TRUE)
str(xm511)
attach(xm511)
panel01<- lm(DAAA~ DUS3MT)
summary(panel01)
library(sandwich)
library(lmtest)
hac_1<- coeftest(panel01, df = Inf, vcov = NeweyWest(panel01, lag = 5, prewhite = FALSE))
hac_1
### Compare with the panel 2 (p.360)                                        ###
z test of coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) 0.0063933  0.0082952  0.7707   0.4409    
DUS3MT      0.2745850  0.0211519 12.9816   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

