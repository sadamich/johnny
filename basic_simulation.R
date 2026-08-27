### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exhibit 1 12 (p.47)

population<- rnorm(10, 0,1)
my_experiment <- NULL
for (i in 1:30) {
  my_sample <- sample(population, size = 5)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
 Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.50474 -0.10165  0.04032  0.16616  0.55182  0.92263 
hist(my_experiment)

my_experiment <- NULL
for (i in 1:300) {
  my_sample <- sample(population, size = 5)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
 Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.77259 -0.15464  0.07573  0.07923  0.34400  0.92263 
hist(my_experiment)
### Exhibit 1 12 (a) (p.47)
par(mfrow = c(2,2))
my_experiment <- NULL
for (i in 1:10000) {
  my_sample <- rnorm(10,0,1)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.74307 -0.05245  0.19382  0.19337  0.44466  1.13840
hist(my_experiment)

### Exhibit 1 12 (b) (p.47)
my_experiment <- NULL
for (i in 1:10000) {
  my_sample <- rnorm(10,0,1)
  my_experiment <- c(my_experiment, median(my_sample))
  cat(sprintf("Sample number %s has a median of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
hist(my_experiment)

### Exhibit 1 12 (c)
my_experiment <- NULL
for (i in 1:10000) {
  my_sample <- rnorm(30,0,1)
  my_experiment <- c(my_experiment, var(my_sample))
  cat(sprintf("Sample number %s has a variance of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
hist(my_experiment)

### Exhibit 1 12 (d)

my_experiment <- NULL
for (i in 1:10000) {
  my_sample <- rnorm(30,0,1)
  my_experiment <- c(my_experiment, 9/10*var(my_sample))
  cat(sprintf("Sample number %s has a ml-variance of %s.\n", i, round(mean(my_sample), 2)))
}
summary(my_experiment)
hist(my_experiment)

