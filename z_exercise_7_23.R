### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 23 (p.720)                                                  ###
xr723 <- read.csv("xr723.csv", header =TRUE)
str(xr723)
attach(xr723)
### a Graphical interpretation                                             ###
g<- ts(P_G, freq = 12, start = 1957)
plot(g, main ="Time series", ylab="Mark")
uk<- ts(P_UK, freq = 12, start = 1957)
plot(uk, main = "Time series", ylab = "Pound")
x_g<- ts(X_G, freq= 12, start = 1957)
plot(x_g, main ="Time series",ylab = "Exchange course")
x_uk<- ts(X_UK, freq= 12, start = 1957)
plot(x_uk, main ="Time series",ylab = "Exchange course")
