### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm101<- read.csv("xm101.csv", header=TRUE)
str(xm101)
attach(xm101)
### Test for the mean (p.62)
t.test(FGPA,mu = 2.7, alternative="greater")
        One Sample t-test
data:  FGPA
t = 4.9757, df = 608, p-value = 4.236e-07
alternative hypothesis: true mean is greater than 2.7
95 percent confidence interval:
 2.762073      Inf
sample estimates:
mean of x 
 2.792796 
