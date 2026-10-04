### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
const<- rep(1, 474)
X<- cbind(const,LOGSAL,EDUC,LOGSALBEGIN)
XX<- t(X)%*%X
XX
               const   LOGSAL     EDUC LOGSALBEGIN
const        474.000  4909.12  6395.00    4583.298
LOGSAL      4909.120 50917.41 66609.45   47527.044
EDUC        6395.000 66609.45 90215.00   62165.992
LOGSALBEGIN 4583.298 47527.04 62165.99   44376.649
cor(LOGSAL,EDUC)
[1] 0.69674
cor(LOGSAL,LOGSALBEGIN)
[1] 0.8863679
cor(EDUC,LOGSALBEGIN)
[1] 0.6857191
c1<- c(1, 0.69674, 0.8863679)
c2<- c(0.69674, 1, 0.6857191)
c3<- c(0.8863679, 0.69674, 1)
r<- cbind(c1,c2,c3)
r

eq<- lm(LOGSAL~EDUC+LOGSALBEGIN)
summary(eq)

res<- resid(eq)
orth<- lm(res~EDUC+LOGSALBEGIN)
summary(orth)