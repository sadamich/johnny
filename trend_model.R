### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
t <- 1:136
y_trend<- lm(Y[45:180] ~ t)
summary(y_trend)
### Compare with panel1(p.591) ###
### Linear forecast ###
f<- function(a, b){
result <- a + b*t
return(result)
}
fore<- f(3.7739920,0.0071399)
fore
plot(fore)

y_dfr<- data.frame(t = 137:150, yt = rep(0, 14))
y_prog<- predict(y_trend, newdata= y_dfr)
plot(Y[45:180], xlab = "Time", main = "Linear Trend")
lines(ts(fitted(y_trend)))
lines(ts(y_prog,start = 137))

dy<- Y[45:180] - Y[44:179]
y_diff<- lm(dy ~ 1)
### Panel4(p.591)              ###
summary(y_diff)
