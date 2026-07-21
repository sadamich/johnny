### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)
str(xm609)

library("survival")


survreg(Surv(STRIKEDUR, STRIKEDUR<80) ~ PROD, xm609, dist='weibull',
                                    scale=1)

survreg(Surv(STRIKEDUR, STRIKEDUR<80) ~ 1, xm609, dist='weibull',
                                    scale=1)


survreg(Surv(STRIKEDUR, STRIKEDUR<80) ~ PROD, xm609, dist='exponential')
                                    