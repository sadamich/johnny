### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 20 (p.718)                                                  ###
xr720 <- read.csv("xr720.csv", header =TRUE)
str(xr720)
attach(xr720)
### a Graphical interpretation                                             ###
### Exponential trend                                                      ###
Y <- USA
Y <- ts(Y, freq = 1, start = 1870)
plot(Y, main = "Time series", ylab = "GNP")
### Linear trend                                                           ###
y <- LOGUSA
y <- ts(y, freq = 1, start = 1870)
plot(y, main = "Time series", ylab = "GNP")
### b Three intervals                                                      ###
OBS
y_1<- y[1:60]
y_1<- ts(y_1, freq = 1, start = 1870)
plot(y_1, main = "1870 - 1929", ylab = "GNP") 
### World wars and restruction after wars                                  ###
y_2<- y[31:80]
y_2<- ts(y_2, freq = 1, start = 1900)
plot(y_2, main = "1900 - 1949", ylab = "GNP")  
y_3<- y[81:124]
y_3<- ts(y_3, freq = 1, start = 1950)
plot(y_3, main = "1950 - 1993", ylab = "GNP")                                            