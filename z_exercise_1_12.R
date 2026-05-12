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
library(maxLik)
l<- function(p){
female<- 236
male<- 373
female*log(p)+male*log(1-p)
}
m<- maxLik(l, start=c(0.6))
summary(m)

beobH<- 6/10
var(FEM)
1/609*p*(1-p)
0.024


erwH<- 236/609
residuum<- p - erwH
cbind(p,erwH, residuum)
prop.test(x = beobH, n= 10, p= 236/609, alternative="less")


s1<- sample(FEM,5)
mean(s1)
[1] 0.6
s2<- sample(FEM,5)
mean(s2)
[1] 0.8
s3<- sample(FEM,5)
mean(s3)
[1] 0.6
s4<- sample(FEM,5)
mean(s4)
[1] 0.6
s5<- sample(FEM,5)
mean(s5)
[1] 0.4

### Repeating                                                              ###
sampling<- c(0.6,0.8,0.6,0.6,0.4)
mean(sampling)
[1] 0.6
sd(sampling)
[1] 0.1414214

t.test(sampling, mu=234/609,alternative="greater")
   One Sample t-test
data:  sampling
t = 3.4115, df = 4, p-value = 0.01349
alternative hypothesis: true mean is greater than 0.3842365
95 percent confidence interval:
 0.4651702       Inf
sample estimates:
mean of x 
      0.6 

xm101<- read.csv("xm101.csv", header=TRUE)
str(xm101)
attach(xm101)
fe<- FGPA[FEM==1]

### mean simulation                                                        ###
my_experiment <- NULL
for (i in 1:100) {
  my_sample <- sample(fe, size = 30)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
my_experiment
summary(my_experiment)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.682   2.819   2.890   2.883   2.942   3.049 
hist(my_experiment)

### t values simulation                                                    ###
my_experiment2 <- NULL
for (i in 1:100) {
  my_sample2 <- sample(fe, size = 30)
  my_experiment2 <- c(my_experiment2, (my_sample2 - mean(my_sample2))/sd(my_sample2))
cat(sprintf("Sample number %s has t value of %s.\n", i, round((my_sample2 - mean(my_sample2))/sd(my_sample2), 2)))            
}
my_experiment2
summary(my_experiment2)
   Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
-2.6379 -0.7121  0.0145  0.0000  0.6885  2.4773 
hist(my_experiment2)

s1<- sample(fe, 30)
mean(s1)
[1] 2.827567
s2<- sample(fe,30)
mean(s2)
[1] 2.894833
s3<- sample(fe,30)
mean(s3)
[1] 3.0493
s4<- sample(fe,30)
mean(s4)
[1] 2.910833
s5<- sample(fe,30)
mean(s5)
[1] 2.8392
s6<- sample(fe,30)
mean(s6)
[1] 2.821467
s7<- sample(fe,30)
mean(s7)
[1] 2.926167
s8<- sample(fe,30)
mean(s8)
[1] 2.962767
s9<- sample(fe,30)
mean(s9)
[1] 3.084267
s10<- sample(fe,30)
mean(s10)
[1] 2.8677
sampling<- c(2.827567,2.894833,3.0493,2.910833,2.8392,
            2.821467 ,2.926167,2.962767,3.084267,2.8677)
mean(sampling)
[1] 2.91841
hist(sampling)