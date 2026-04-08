### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr528<- read.csv("xr528.csv", header =TRUE)
str(xr528)
attach(xr528)
detach(xr528)

x<- US3MTBIL[2:180] - US3MTBIL[1:179]
str(x)
eq<- lm(x~ US3MTBIL[1:179])
summary(eq)
res<- resid(eq)
eq_var<- lm(res^2 ~ x)
summary(eq_var)
179* 0.06691 = 11.97689