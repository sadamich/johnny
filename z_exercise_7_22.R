### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 22 (p.718)                                                  ###
xr722 <- read.csv("xr722.csv", header =TRUE)
str(xr722)
attach(xr722)
### a Residuals and sqared residuals                                        ###                        
eq1<- lm(RENDCYCO ~ RENDMARK)
summary(eq1)
res<- resid(eq1)
acf(res)
acf(res^2)
### Tests for autocorrelations and ARCH                                     ###
### GARCH                                                                   ###
