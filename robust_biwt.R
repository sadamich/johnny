library(biwt)
### To calculate the multivariate location vector and scale matrix:
samp.data <- t(mvrnorm(30,mu=c(0,0),Sigma=matrix(c(1,.75,.75,1),ncol=2)))
str(samp.data)
 num [1:2, 1:30] -2.6355 -2.7765 0.0441 0.6446 -1.5983 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : NULL
head(samp.data)
plot(samp.data)
x<- samp.data[1, ]
str(x)
num [1:30] -2.6355 0.0441 -1.5983 -0.6721 0.2485 ...
plot(x)
hist(x)
summary(x)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-2.63550 -0.58741  0.04268  0.08847  0.71797  2.43867 
sd(x)
[1] 1.136708
y<- samp.data[2 ,]
str(y)
 num [1:30] -2.777 0.645 -0.852 -0.419 0.951 ...

plot(y)
hist(y)
summary(y)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-2.77651 -0.61647 -0.05815  0.07837  0.63991  3.40910 
sd(y)
[1] 1.178834
plot(x,y)

samp.bw <- biwt.est(samp.data)
samp.bw
summary(samp.bw)
$biwt.mu
[1] 0.04379220 0.05076558
$biwt.sig
          [,1]      [,2]
[1,] 0.6089694 0.5040607
[2,] 0.5040607 0.6122468

samp.bw.var1 <- samp.bw$biwt.sig[1,1]
samp.bw.var2 <- samp.bw$biwt.sig[2,2]
samp.bw.cov <- samp.bw$biwt.sig[1,2]

samp.bw.cor <- samp.bw$biwt.sig[1,2] / 
	sqrt(samp.bw$biwt.sig[1,1]*samp.bw$biwt.sig[2,2])
samp.bw.cor
[1] 0.8255091

### To calculate the correlation(s):
samp.data <- t(mvrnorm(30,mu=c(0,0,0),
	Sigma=matrix(c(1,.75,-.75,.75,1,-.75,-.75,-.75,1),ncol=3)))
str(samp.data)
 num [1:3, 1:30] 1.3719 1.6614 -2.3521 -0.0702 -0.7181 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : NULL
x<- samp.data[1 , ]
plot(x)
hist(x)
y<- samp.data[2 , ]
plot(y)
hist(y)
z<- samp.data[3 , ]
plot(z)
hist(z)
# To compute the 3 pairwise correlations from the sample data:
samp.bw.cor <- biwt.cor(samp.data, output="vector")
samp.bw.cor
[1]  0.5375758 -0.5944700 -0.6273646

# To compute the 3 pairwise correlations in matrix form:
samp.bw.cor.mat <- biwt.cor(samp.data)
samp.bw.cor.mat
           [,1]       [,2]       [,3]
[1,]  1.0000000  0.5375758 -0.5944700
[2,]  0.5375758  1.0000000 -0.6273646
[3,] -0.5944700 -0.6273646  1.0000000

# To compute the 3 pairwise distances in matrix form:
samp.bw.dist.mat <- biwt.cor(samp.data, output="distance")
samp.bw.dist.mat
          [,1]      [,2]      [,3]
[1,] 0.0000000 0.4624241 0.4055300
[2,] 0.4624241 0.0000000 0.3726355
[3,] 0.4055300 0.3726355 0.0000000

# To convert the distances into an object of class `dist'
as.dist(samp.bw.dist.mat)
          1         2
2 0.4624241          
3 0.4055300 0.3726355
> 

