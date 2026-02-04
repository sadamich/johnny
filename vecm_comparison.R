### Compare with the Example 7 27 Interest and bond rates p.674            ###
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
library(urca)
head(xm722)
ir <- xm722[, c("AAA", "US3MTBIL")]
str(ir)
ir.vecm <- ca.jo(ir, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
summary(ir.vecm)
summary(alphaols(ir.vecm))
summary(alphaols(ir.vecm, reg.number=1))
lttest(ir.vecm, r=1)
plot(ir.vecm)
plotres(ir.vecm)