### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm404<- read.csv("xm404.csv", header = TRUE)
attach(xm404)
str(xm404)

library(car)
https://cran.r-project.org/web/packages/car/refman/car.html#dfbetaPlots
dfbetaPlots(lm(RENDCYCO ~ RENDMARK, data=xm404))

https://cran.r-project.org/web/packages/car/refman/car.html#influencePlot
influencePlot(lm(RENDCYCO ~ RENDMARK, data=xm404))
     StudRes         Hat       CookD
9  -3.7954106 0.005492563 0.037658009
16  2.7791941 0.010024233 0.038030750
21  0.4737615 0.059313776 0.007099337
33 -3.6260039 0.004725594 0.029697522
94 -0.4862783 0.156308815 0.021975319

https://cran.r-project.org/web/packages/car/refman/car.html#leveragePlots
leveragePlots(lm(RENDCYCO ~ RENDMARK, data=xm404))

https://cran.r-project.org/web/packages/car/refman/car.html#mmps
Marginal Model Plotting
m1<- lm(RENDCYCO ~ RENDMARK, data=xm404)
mmps(m1)