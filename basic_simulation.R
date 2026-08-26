### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exhibit 1 12 (p.47)

set.seed(21)
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
