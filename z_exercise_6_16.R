### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 16 (p. 530)   Marketing

xm601<- read.csv("xm601.csv", header=TRUE)
str(xm601)
attach(xm601)
### Example 6 7 A tobit model
### Example 6 2 A binary probit model
panel03<- glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, 
family = binomial(link = "probit"))
summary(panel03)

### Problem (a) the comparison of the censored with the binary probit model


### Problem (b) better methods?

detach(xm601)

