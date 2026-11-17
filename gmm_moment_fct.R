### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
https://cran.r-project.org/web/packages/gmm/index.html
### Example 1 7 (p.39)and compare with 4 4 2 (p. 252)                      ###
xm101<- read.csv("xm101.csv", header=TRUE)
attach(xm101)
str(xm101)
y<- FGPA
mu<- mean(FGPA)
mu
[1] 2.792796
sd(y)
[1] 0.4602375
### The sample moment function                                             ###
m<- function(r){
result<- 1/609*sum(((y-mu)^r))
return(result)
}
### The sample moments                                                     ###
m(2)
[1] 0.2114708
m(3)
[1] 0.0163208
m(4)
[1] 0.1122702
### The sample skewness                                                    ###
(0.0163208)/(0.4602375^3)
[1] 0.1674154
### The sample kurtsis                                                     ###
(0.1122702)/(0.4602375^4)
[1] 2.502283






