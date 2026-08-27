### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Central-Limit-Theorem/Population-Statistics-and-Sample-Statistics/index.html
xm101<- read.csv("xm101.csv", header=TRUE)
str(xm101)
attach(xm101)

### Example 1 13 (p.66)
pop<- FGPA[FEM==1]
my_experiment <- NULL
for (i in 1:1000) {
  my_sample <- sample(pop, size = 10)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.354   2.795   2.901   2.899   2.995   3.478 
hist(my_experiment)
t.test(my_experiment)
       One Sample t-test
data:  my_experiment
t = 612.84, df = 999, p-value < 2.2e-16
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
 2.890049 2.908617
sample estimates:
mean of x 
 2.899333 


### The sampling
population<- FGPA
my_sample <- sample(x = population, size = 10)
my_sample
[1] 3.200 2.773 2.543 2.703 2.770 3.226 2.509 3.728 1.934 2.793
x_bar <- mean(my_sample)
x_bar
[1] 2.8179
s <- sd(my_sample)
s
[1] 0.4843356

my_experiment <- NULL
for (i in 1:5) {
  my_sample <- sample(population, size = 10)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
Sample number 1 has a mean of 2.87.
Sample number 2 has a mean of 2.62.
Sample number 3 has a mean of 2.59.
Sample number 4 has a mean of 2.5.
Sample number 5 has a mean of 2.88.
summary(my_experiment)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.498   2.587   2.622   2.692   2.873   2.881 
hist(my_experiment)

### The sample size 10 : Trials 300
my_experiment <- NULL
for (i in 1:300) {
  my_sample <- sample(population, size = 10)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.338   2.695   2.794   2.790   2.886   3.189 
hist(my_experiment)
t.test(my_experiment)
      One Sample t-test

data:  my_experiment
t = 338.39, df = 299, p-value < 2.2e-16
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
 2.774099 2.806553
sample estimates:
mean of x 
 2.790326 

### The sample size 30 : Trials 300
my_experiment <- NULL
for (i in 1:300) {
  my_sample <- sample(population, size = 30)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.588   2.755   2.801   2.801   2.853   3.047 
hist(my_experiment)
t.test(my_experiment)
   One Sample t-test
data:  my_experiment
t = 639.06, df = 299, p-value < 2.2e-16
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
 2.792862 2.810116
sample estimates:
mean of x 
 2.801489 

### The sample size= 30 : trials = 1000
my_experiment <- NULL
for (i in 1:1000) {
  my_sample <- sample(population, size = 30)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.549   2.739   2.794   2.793   2.848   3.061 
hist(my_experiment)
t.test(my_experiment)

### The sample size= 10 : trials = 10000
my_experiment <- NULL
population<- FGPA
for (i in 1:10000) {
  my_sample <- sample(population, size = 10)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
 
hist(my_experiment)
t.test(my_experiment)


### The confidence interval
lower_90 <- qnorm(0.05, lower.tail = TRUE)
upper_90 <- qnorm(0.05, lower.tail = FALSE)

paste(
  "The lower and upper limits of the interval that covers an area of 90% around the mean are given by z-scores of",
  round(lower_90, 2), "and", round(upper_90, 2), "respectively."
)
[1] "The lower and upper limits of the interval that covers an area 
of 90% around the mean are given by z-scores of -1.64 and 1.64 respectively."



lower_95 <- qnorm(0.025, lower.tail = TRUE)
upper_95 <- qnorm(0.025, lower.tail = FALSE)
paste(
  "The lower and upper limits of the interval that covers an area of 95% around the mean are given by z-scores of",
  round(lower_95, 2), "and", round(upper_95, 2), "respectively."
)

[1] "The lower and upper limits of the interval that covers an area of 95% 
around the mean are given by z-scores of -1.96 and 1.96 respectively."

lower_99 <- qnorm(0.005, lower.tail = TRUE)
upper_99 <- qnorm(0.005, lower.tail = FALSE)
paste(
  "The lower and upper limits of the interval that covers an area of 95% around the mean are given by z-scores of",
  round(lower_99, 2), "and", round(upper_99, 2), "respectively."
)
[1] "The lower and upper limits of the interval that covers an area of 
95% around the mean are given by z-scores of -2.58 and 2.58 respectively."






library(bootstrap)
n<- 609
y<- FGPA
theta<- function(y){
result<- mean(y)/(sd(y)/sqrt(n))
return(result)
}
z<- bootstrap(y, 10000,theta)
perc95<- function(y){quantile(y, .95)}
results <-  bootstrap(y,10000,theta, func=perc95) 
results

