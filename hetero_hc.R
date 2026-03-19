### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 11 and 5 12 Interest and Bond rates p. 322 and p.325         ###
xm501<- read.csv("xm501.csv", header = TRUE)
str(xm501)
attach(xm501)
panel01<- lm(LOGSALARY~EDUC+GENDER+MINORITY+DUMJCAT2+DUMJCAT3)
coeftest(panel01, df = Inf, vcov = vcovHC(panel01, type = "HC1"))
### Panel 1 (p.326) z test of coefficients:                                ###
              Estimate Std. Error  z value  Pr(>|z|)    
(Intercept)  9.5746941  0.0544773 175.7556 < 2.2e-16 ***
EDUC         0.0441917  0.0044245   9.9879 < 2.2e-16 ***
GENDER       0.1783403  0.0199847   8.9238 < 2.2e-16 ***
MINORITY    -0.0748581  0.0206988  -3.6165 0.0002986 ***
DUMJCAT2     0.1703598  0.0330252   5.1585  2.49e-07 ***
DUMJCAT3     0.5390753  0.0358870  15.0215 < 2.2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

xm511<- read.csv("xm511.csv", header = TRUE)
str(xm511)
attach(xm511)
library(sandwich)
library(lmtest)
eqols<- lm(DAAA~DUS3MT)
coeftest(eqols,  df = Inf, vcov = vcovHC(eqols, type = "HC0"))
z test of coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) 0.0063933  0.0069807  0.9158   0.3597    
DUS3MT      0.2745850  0.0228361 12.0242   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

hc_1 <-coeftest(eqols, df = Inf, vcov = vcovHC(eqols, type = "HC1"))
hc_1
### Panel 4 (p.326) z test of coefficients:                                 ###
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) 0.0063933  0.0069924  0.9143   0.3605    
DUS3MT      0.2745850  0.0228743 12.0041   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 