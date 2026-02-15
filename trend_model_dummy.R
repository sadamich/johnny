### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
### Example 7 17 (p.615)                                                   ###
panel02<- lm(D4Y[45:180]~D4Y[44:179]+D4Y[43:178]+DUM611[45:180]+DUM612[45:180]
+DUM744[45:180]+DUM751[45:180]+DUM761[45:180]+DUM802[45:180]+DUM814[45:180])
summary(panel02)
res_d<- resid(panel02)
### Exhibit 7 21 c (p.615)                                                 ###
hist(res_d)
summary(res_d)
library(tseries)
jarque.bera.test(res_d)
  Jarque Bera Test
data:  res_d
X-squared = 0.88824, df = 2, p-value = 0.6414
### The normality is not rejected                                          ###
ar2<- lm(D4Y[45:180]~D4Y[44:179]+D4Y[43:178])
res_ar2<- resid(ar2)
jarque.bera.test(res_ar2)
    Jarque Bera Test
data:  res_ar2
X-squared = 47.784, df = 2, p-value = 4.206e-11
### The normality is rejected                                              ###



