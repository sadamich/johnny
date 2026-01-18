### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
 

### Example 7 8 Industrial Production  p.562 ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
D4Y_60<- D4Y[45:180]
### AR(2) Model ###
d4_1<- D4Y[44:179]
d4_2<- D4Y[43:178]
ar_2<- lm(D4Y_60 ~ d4_1+d4_2)
summary(ar_2)
Exhibit 7.9 (p.563) Call:lm(formula = D4Y_60 ~ d4_1 + d4_2)