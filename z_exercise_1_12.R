### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 1 12 (p.73)                                                   ###
xr111<- read.csv("xr111.csv", header=TRUE)
str(xr111)
attach(xr111)

### Problem (a)                                                            ###
t<- function(mean, mu, sd){
result<- (mean - mu)/sd
return(result)
}
t(mean(FGPA), 2.79,sd(FGPA))
[1] -0.1875647
t.test(FGPA,mu=2.79)
t.test(FGPA, mu = 2.79, alternative= "less")
     One Sample t-test
data:  FGPA
t = -0.59313, df = 9, p-value = 0.2838
alternative hypothesis: true mean is less than 2.79
95 percent confidence interval:
     -Inf 3.001565
sample estimates:
mean of x 
   2.6888 
### S.329
t.test(FGPA)$conf.int
[1] 2.302831 3.074769
attr(,"conf.level")
[1] 0.95
### S. 331

### Problem (b)
   One Sample t-test
data:  FGPA
t = -0.59313, df = 9, p-value = 0.5677
alternative hypothesis: true mean is not equal to 2.79
95 percent confidence interval:
 2.302831 3.074769
sample estimates:
mean of x 
   2.6888 
### Problem (c)

chi<- function(s_sq, sigma_s,n){
result<- (n-1)*s_sq/sigma_s
return(result)
}
chi(0.2911111, 0.2116,10)
[1] 12.38185

pchisq(12.38185, 9)
[1] 0.8073718
P-value
1 - pchisq(12.38185, 9)
[1] 0.1926282

var(FGPA)
[1] 0.2911111 (the sample variance) 
0.46^2
[1] 0.2116 (the thoretical variance)

### Problem (d)

beobH<- 6/10
var(FEM)
1/609*p*(1-p)
0.024


erwH<- 236/609
residuum<- p - erwH
cbind(p,erwH, residuum)
prop.test(x = beobH, n= 10, p= 236/609, alternative="less")