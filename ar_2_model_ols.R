### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 

### Example 7 8 Industrial Production  p.562 ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
D4Y_60<- D4Y[45:180]
### AR(2) Model ###
d4_1<- D4Y[44:179]
d4_2<- D4Y[43:178]
ar_2<- lm(D4Y_60 ~ d4_1+d4_2)
summary(ar_2)
### Exhibit 7.9 (p.563) Call:lm(formula = D4Y_60 ~ d4_1 + d4_2)            ###
library(forecast)
Acf(res, main = "ACF")
### ARCH LM test ###
res <- resid(ar_2)
res_lag <- c(NA, res)
res_lag2<- c(NA, res_lag)
res_lag3<- c(NA, res_lag2)
res_lag4<- c(NA, res_lag3)
panel02 <- lm(res ~ d4_1+ d4_2 + res_lag[1:136]
+ res_lag2[1:136]+res_lag3[1:136]+ res_lag4[1:136])
summary(panel02)

### Compare with the exhibit 7. 11 Panel 3 and Panel 4 (p.573)
