### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)

x<- STRIKEDUR

hist(x)
summary(x)

   Min.  1st Qu.  Median   Mean  3rd Qu.    Max. 
   1.00   10.25   27.00   42.68   51.25  216.00

x1<- x[x<=10.25]
str(x1)
int [1:16] 1 2 2 2 3 3 3 3 3 4 ...
x2<- x[x<=27]
str(x2)
int [1:32] 1 2 2 2 3 3 3 3 3 4 ...
x3<- x[x<=51.25]
str(x3)
 int [1:46] 1 2 2 2 3 3 3 3 3 4 .
x4<- x[x<=216]
str(x4)
int [1:62] 1 2 2 2 3 3 3 3 3 4 ...
quan<- c(x1,x2,x3,x4)
q<- c(16,16,14,16)
plot(q)
