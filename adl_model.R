### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 22 Interest and bond rates p.642 ###
xm722<- read.csv("xm722.csv", header = TRUE) 
str(xm722)
attach(xm722)
library("dLagM")
DUS3MT_50<- DUS3MT[25:624]
DAAA_50<- DAAA[25:624]
eq_adl<- ardlDlm(x = DUS3MT_50, y = DAAA_50, p = 4, q= 3)
summary(eq_adl)
