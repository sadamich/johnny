### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 12 (p. 528) Marketing: The Group 

xm601<- read.csv("xm601.csv", header=TRUE)
str(xm601)
attach(xm601)
group1<- subset(xm601, AGE<=30)
group2<- subset(xm601, 30< AGE <=35)
group3<- subset(xm601, AGE > 35, AGE <=40)
group4<- subset(xm601, AGE > 40, AGE <= 45)
group5<- subset(xm601, AGE > 45, AGE <= 50)
group6<- subset(xm601, AGE > 50, AGE <= 55)
group7<- subset(xm601, AGE > 55, AGE <= 60)
group8<- subset(xm601, AGE > 60, AGE <= 65)
group9<- subset(xm601, AGE > 65, AGE <= 70)
group10<- subset(xm601, AGE > 70, AGE <= 75)
### Problem (a) 10 Groups  The Logit model

### Problem (b) The Logit model

### Problem (c) The Logit model with restrictions

### Problem (d) The Logit model with FWLS estimate

### Problem (e) Comparison with b and d






detach(xm601)