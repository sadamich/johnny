### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
### Graphs of production                                                   ###
x<- ts(X, freq = 4, start = 1950)
plot(x, main = "Time", ylab = "Production")
y<- ts(Y, freq = 4, start = 1950)
plot(y, main = "Time", ylab = "log Production")
lag4<- c(NA,NA,NA,NA,Y)
growth<- D4Y/lag4[1:195]
growth_ts<- ts(growth,freq=4, start =1950) 
plot(growth_ts, main="Time", ylab="Growth rates")
### Linear trend model                                                     ###  
t <- 1:136
y_trend<- lm(Y[45:180] ~ t)
summary(y_trend)
### Compare with panel1(p.591)                                             ###
### Linear forecast                                                        ###
f<- function(a, b){
result <- a + b*t
return(result)
}
fore<- f(3.7739920,0.0071399)
fore
plot(fore)


y_61<- ts(Y[45:180], freq = 4, start = 1961)
plot(y_61, main = "Time", ylab = "log Production")
y_dfr<- data.frame(t = 137:150, yt = rep(0, 14))
y_prog<- predict(y_trend, newdata= y_dfr, freq=4, start = 1961)
plot(y_61, xlab = "Time", main = "Linear Trend", xlim= c(1961, 2000))
lines(ts(fitted(y_trend), freq= 4, start = 1961))
lines(ts(y_prog,freq= 4,start = 1995))

dy<- Y[45:180] - Y[44:179]
y_diff<- lm(dy ~ 1)
### Panel4(p.591)              ###
summary(y_diff)
