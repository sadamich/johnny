https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Central-Limit-Theorem/Sampling-a-Normally-Distributed-Population/index.html
trials <- 1000
n <- c(5, 15, 30, 50) # sample size
# empty matrix to store results of computations
out <- matrix(nrow = trials, ncol = length(n))

# random sampling
for (i in seq(trials)) {
  for (j in seq(length(n))) {
    out[i, j] <- mean(rnorm(n[j]))
  }
}
out[1,1]
out[2,1]
out[1000,1]
mean(out[i,1])
mean(out[i,2])
mean(out[i,3])
mean(out[i,j])
mean(out[i,51])
Error in out[i, 51] : subscript out of bounds

mean(out[1001,1])
Error in out[1001, 1] : subscript out of bounds


https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Central-Limit-Theorem/Sampling-a-Population-not-Normally-Distributed/index.html

trials <- 1000
n <- c(2, 5, 15, 30) # sample size
# empty matrix to store results of computations
out_unif <- matrix(nrow = trials, ncol = length(n))
out_beta <- matrix(nrow = trials, ncol = length(n))
out_gamma <- matrix(nrow = trials, ncol = length(n))

# random sampling
for (i in seq(trials)) {
  for (j in seq(length(n))) {
    out_unif[i, j] <- mean(runif(n[j], min = 0.2, max = 0.8))
    out_beta[i, j] <- mean(rbeta(n[j], shape1 = 2, shape2 = 5))
    out_gamma[i, j] <- mean(rgamma(n[j], shape = 1, rate = 8))
  }
}
out_unif[1,1]
out_beta[1,1]
out_gamma[1,1]
out_unif[1000,1]
out_unif[1001,1]
Error in out_unif[1001, 1] : subscript out of bounds
out_unif[1000,5]
Error in out_unif[1000, 5] : subscript out of bounds
out_unif[1000,4]
[1] 0.5074815