### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 15 (p. 529)   Microeconomics

xr615<- read.csv("xr615.csv", header=TRUE)
str(xr615)
attach(xr615)
### Problem (a) the ordered Logit model 
str(LEVELMATH)
y<- LEVELMATH
head(y, 300)
sat<- SATMATH/100
eq_logit<- glm(formula = y~ sat+FEMALE+MAJORESH+MAJORNAT+ADVMATH1+
                            ADVMATH2+ADVMATH3+PHYSICS+CHEMISTRY, 
family = binomial)
summary(eq_logit)???

### Problem (b) the comparison of the ordered logit with the probit model

### Problem (c) the MNL mode




detach(xr615)