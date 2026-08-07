### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)
str(xm609)

t <- STRIKEDUR
library("survival")
library(censReg)
### Panel 12 (p.519) 
censReg(log(STRIKECENS80)~1, left = 0, right = 80, data = xm609)
Call:
censReg(formula = log(STRIKECENS80) ~ 1, left = 0, right = 80, 
    data = xm609)
Coefficients:
(Intercept)    logSigma 
     3.0198      0.1826 
exp(0.1826)
[1] 1.200334
censReg(log(STRIKECENS80)~PROD, left = 0, right = 80, data = xm609)
Call:
censReg(formula = log(STRIKECENS80) ~ PROD, left = 0, right = 80, 
    data = xm609)
Coefficients:
(Intercept)        PROD    logSigma 
     3.1091     -8.1121      0.1322 
exp( 0.1322)
[1] 1.141337