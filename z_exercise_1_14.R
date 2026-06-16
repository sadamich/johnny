### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 1 14 (p.74)

Usage
dt(x, df, ncp, log = FALSE)
pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
rt(n, df, ncp)

### Problem (a)                                                            ###
set.seed(13)
y<- rt(10,3)
y_mean<- mean(y)
y_sd<- sd(y)

y_mean + 2*y_sd/sqrt(10)
[1] 0.8014445
y_mean - 2*y_sd/sqrt(10)
[1] -0.8996569

### Problem (b)
set.seed(15)
y<- rt(10,3)
y_mean<- mean(y)
y_sd<- sd(y)
y_mean + 2*y_sd/sqrt(10)
[1] 1.852381
y_mean - 2*y_sd/sqrt(10)
[1] -0.1064735
set.seed(17)
y<- rt(10,3)
y_mean<- mean(y)
y_sd<- sd(y)
y_mean + 2*y_sd/sqrt(10)
[1] 0.7119812
y_mean - 2*y_sd/sqrt(10)
[1] -0.268698

### Bootstraping confidence interval
n<- 10
y<- rt(10,3)
values<- NULL
t_sta<- NULL
average<-NULL
sterror<- NULL
c_int<- NULL
for (i in 1:10000){
values<- c(values, sample(y,n))
average<- c(average,mean(values))
sterror<- c(sterror,sd(values))
t_sta<- c(t_sta,average/(sterror/sqrt(n)))
}
library(boot)
n<- 10
set.seed(23)
y<- rt(10,3)
theta<- function(y){
result<- mean(y)
return(result)
}
y.boot <- boot(y,theta,R = 100, sim = "parametric")
boot.ci(y.boot, conf = c(0.90, 0.95),
        type = c("basic"))
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 100 bootstrap replicates
CALL : 
boot.ci(boot.out = y.boot, conf = c(0.9, 0.95), type = c("basic"))
Intervals : 
Level      Basic         
90%   (-0.0572, -0.0572 )   
95%   (-0.0572, -0.0572 )  
Calculations and Intervals on Original Scale
Some basic intervals may be unstable



n<- 100
y<- rt(10,3)
theta<- function(y){
result<- mean(y)/(sd(y)/sqrt(n))
return(result)
}
y.boot <- boot(y,theta,R = 10000, sim = "parametric")
boot.ci(y.boot, conf = c(0.90, 0.95),
        type = c("norm", "basic"))





library(bootstrap)
n<- 10
y<- rt(10,3)
theta<- function(y){
result<- mean(y)/(sd(y)/sqrt(n))
return(result)
}
z<- bootstrap(y, 10000,theta)
perc95<- function(y){quantile(y, .95)}
results <-  bootstrap(y,10000,theta, func=perc95) 
results
### Problem (c)
n = 100,n= 1000

### Problem random sampling with mean statistics                           ###
### trials = 30 , population= 10, sample size = 5
set.seed(30)
population <- rt(10,3)
my_experiment <- NULL
for (i in 1:30) {
  my_sample <- sample(population, size = 5)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}

### Trials = 100 , population = 100, sample size = 30 
set.seed(35)
population <- rt(100,3)
my_experiment <- NULL
for (i in 1:100) {
  my_sample <- sample(population, size = 30)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}



