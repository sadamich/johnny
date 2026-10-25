### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)

influencePlot(lm(LOGSAL ~ EDUC + LOGSALBEGIN+GENDER+MINORITY, data=xm301))
        StudRes         Hat        CookD
18   3.73956464 0.007648883 0.0209770625
29  -0.20203972 0.061938991 0.0005401640
111  2.80606641 0.021983811 0.0348870048
218  5.07649711 0.007189386 0.0354512107
343  0.03317241 0.048902995 0.0000113402

## Not run:  # requires user interaction to identify points
influencePlot(lm(LOGSAL ~ EDUC + LOGSALBEGIN+GENDER+MINORITY, data=xm301), 
    id=list(method="identify"))