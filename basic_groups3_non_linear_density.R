### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)
Nonparametric Density Estimates
densityPlot(~ EDUC, show.bw=TRUE, method="kernel", data=xm301)
densityPlot(~ EDUC, show.bw=TRUE, data=xm301)
densityPlot(~ EDUC, from=0, normalize=TRUE, show.bw=TRUE, data=xm301)

densityPlot(EDUC ~ type, data=xm301)
densityPlot(~ EDUC, show.bw=TRUE, method="kernel", data=xm301)
densityPlot(~ EDUC, show.bw=TRUE, data=xm301)
densityPlot(~ EDUC, from=0, normalize=TRUE, show.bw=TRUE, data=xm301)


educ<- factor(EDUC,labels=c("NA","12","14","15","16","17","NA","NA","NA","NA"))
densityPlot(LOGSAL ~educ,kernel=depan,data=xm301)

educ2<- factor(EDUC,labels=c("NA","A","A","A","B","B","C","C","NA","NA"))
densityPlot(LOGSAL ~educ2,kernel=depan,data=xm301)

educ3<- factor(EDUC,labels=c("NA","A","A","A","B","B","B","C","C","NA"))
densityPlot(LOGSAL ~educ3,kernel=depan,data=xm301)

densityPlot(LOGSAL ~ educ, kernel=depan, legend=list(location="top"), data=xm301)

plot(adaptiveKernel(UN$infantMortality, from=0, adjust=0.75), col="magenta")
lines(density(na.omit(UN$infantMortality), from=0, adjust=0.75), col="blue")
rug(UN$infantMortality, col="cyan")
legend("topright", col=c("magenta", "blue"), lty=1,
       legend=c("adaptive kernel", "kernel"), inset=0.02)


plot(adaptiveKernel(UN$infantMortality, from=0, adjust=0.75), col="magenta")
lines(density(na.omit(UN$infantMortality), from=0, adjust=0.75), col="blue")
rug(UN$infantMortality, col="cyan")

