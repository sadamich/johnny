library(REdaS)
library(ggplot2)
str(faithful)
'data.frame':   272 obs. of  2 variables:
 $ eruptions: num  3.6 1.8 3.33 2.28 4.53 ...
 $ waiting  : num  79 54 74 62 85 55 88 85 51 85 ...
ggplot(faithful, aes(waiting, eruptions)) +
  geom_point() +
  stat_ellipse()


aes(x = mpg, y = wt)
aes(mpg, wt)

aes(x = mpg ^ 2, y = wt / cyl)
ggplot(aes)


### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
z<- data.frame(EDUC, LOGSAL)
z<- ggplot(data = z)
ggplot(xm301, aes(EDUC, LOGSAL))+
 geom_point() +
  stat_ellipse()


ggplot(xm301, aes(EDUC, LOGSAL))+
 geom_point() +
geom_smooth(formula = y ~ x, method = "glm")


ggplot(xm301, aes(EDUC,LOGSAL, colour = class))+
 geom_point() +
scale_colour_viridis_d()
????


ggplot(xm301, aes(EDUC, LOGSAL))+
 geom_point() +
 coord_fixed()


ggplot(xm301, aes(EDUC, LOGSAL))+
 geom_point() +
theme_minimal() +
  theme(
    legend.position = "top",
    axis.line = element_line(linewidth = 0.75),
    axis.line.x.bottom = element_line(colour = "blue")
  )