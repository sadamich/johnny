### Source: https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Central-Limit-Theorem/Sampling-Error/index.html ###
### Source: Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Example 1 12, p.62                                                     ### 
xm101<- read.csv("xm101.csv", header =TRUE)
attach(xm101)
str(xm101)
mean(FGPA)
[1] 2.792796  (Population mean)

### Random sampling ###
population <- FGPA
my_experiment <- NULL
for (i in 1:10) {
my_sample <- sample(population, size = 10)
my_experiment <- c(my_experiment, mean(my_sample))
cat(sprintf("Sample nuber %s has a mean of %s.\n", i, round(mean(my_sample),2)))
}
### (Random) sample mean ###
Sample nuber 1 has a mean of 2.73.
Sample nuber 2 has a mean of 2.61.
Sample nuber 3 has a mean of 2.67.
Sample nuber 4 has a mean of 2.97.
Sample nuber 5 has a mean of 2.78.
Sample nuber 6 has a mean of 2.51.
Sample nuber 7 has a mean of 2.86.
Sample nuber 8 has a mean of 2.88.
Sample nuber 9 has a mean of 2.86.
Sample nuber 10 has a mean of 2.64.

### Sampling error   ###
pop_mean <- mean(population)
vector_error_sample_10 <- NULL
vector_error_sample_25 <- NULL
vector_error_sample_50 <- NULL
vector_error_sample_75 <- NULL
trials <- 1000
for (trial in 1:trials) {
  my_sample_10 <- sample(population, 10)
  my_sample_25 <- sample(population, 25)
  my_sample_50 <- sample(population, 50)
  my_sample_75 <- sample(population, 75)

  error_sample_10 <- abs(mean(my_sample_10) - pop_mean)
  error_sample_25 <- abs(mean(my_sample_25) - pop_mean)
  error_sample_50 <- abs(mean(my_sample_50) - pop_mean)
  error_sample_75 <- abs(mean(my_sample_75) - pop_mean)
### the sampling error convergences ###

  vector_error_sample_10 <- c(vector_error_sample_10, error_sample_10)
  vector_error_sample_25 <- c(vector_error_sample_25, error_sample_25)
  vector_error_sample_50 <- c(vector_error_sample_50, error_sample_50)
  vector_error_sample_75 <- c(vector_error_sample_75, error_sample_75)
}

print(paste("Sampling Error, n = 10: ", mean(vector_error_sample_10)))
print(paste("Sampling Error, n = 25: ", mean(vector_error_sample_25)))
print(paste("Sampling Error, n = 50: ", mean(vector_error_sample_50)))
print(paste("Sampling Error, n = 75: ", mean(vector_error_sample_75)))
