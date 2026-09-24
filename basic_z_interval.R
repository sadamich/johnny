### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm101<- read.csv("xm101.csv", header=TRUE)
str(xm101)
attach(xm101)
### https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Inferential-Statistics/z-Distribution/The-One-Mean-z-Interval-Procedure/index.html

students <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/students.csv")
str(students)
females <- subset(students, gender == "Female")
heights <- females$height
hist(heights, breaks = "FD")
heights_mean <- mean(heights)
heights_mean
heights_sigma <- sd(heights)
heights_sigma
x <- seq(min(heights), max(heights), by = 0.01)
height_pdf <- dnorm(x, mean = heights_mean, sd = heights_sigma)
plot(x, height_pdf,type='n')
hist(heights, breaks = "Scott", freq = F, add = T)
lines(x, height_pdf, type = "l", col = "red", ylab = "Density", xlab = "Height in cm")
sample_size <- 10
my_sample <- rnorm(n = sample_size, mean = heights_mean, sd = heights_sigma)
x_bar <- mean(my_sample)
x_bar
x_bar == heights_mean
CI.eval <- function(pop_mean, sigma, n, estimate, alpha) {
  # The function returns a vector of Booleans (TRUE or FALSE).
  # The function returns TRUE if the confidence interval contains the true population parameter and FALSE if not.

  out <- rep(NA, length(alpha))
  for (i in seq(1, length(alpha))) {
    out[i] <- pop_mean >= estimate - qnorm(alpha[i] / 2, lower.tail = F) * sigma / sqrt(n) &&
      pop_mean <= estimate + qnorm(alpha[i] / 2, lower.tail = F) * sigma / sqrt(n)
  }
  return(out)
}
eval <- CI.eval(
  pop_mean = heights_mean,
  sigma = heights_sigma,
  n = sample_size,
  estimate = x_bar,
  alpha = c(0.1, 0.05, 0.01)
)
data.frame(eval, row.names = c("90 %", "95 %", "99 %"))
df <- data.frame()

trials <- c(10, 50, 100, 1000, 10000)
n <- 10

for (trial in trials) {
  m <- matrix(NA, nrow = trial, ncol = 3)
  for (i in 1:trial) {
    my_sample <- rnorm(n = n, mean = heights_mean, sd = heights_sigma)
    x_bar <- mean(my_sample)
    eval <- CI.eval(
      pop_mean = heights_mean,
      sigma = heights_sigma,
      n = n,
      estimate = x_bar,
      alpha = c(0.1, 0.05, 0.01)
    )
    m[i, ] <- eval
  }
  df <- rbind(df, colSums(m) / trial * 100)
}
row.names(df) <- trials
colnames(df) <- c("90 %", "95 %", "99 %")
df

### The Example of FGPA 
hist(FGPA[FEM==1], breaks ="FD")
fgpa_mean<- mean(FGPA[FEM==1])
fgpa_mean
[1] 2.894831
fgpa_sigma<- sd(FGPA[FEM==1])
fgpa_sigma
[1] 0.4719426

x <- seq(min(FGPA[FEM==1]), max(FGPA[FEM==1]), by = 0.01)
fgpa_pdf <- dnorm(x, mean = fgpa_mean, sd = fgpa_sigma)

plot(x, fgpa_pdf,type='n')
hist(FGPA[FEM==1], breaks = "Scott", freq = F, add = T)
lines(x, fgpa_pdf, type = "l", col = "red", ylab = "Density", xlab = "FGPA(FEM)")
sample_size <- 10
my_sample <- rnorm(n = sample_size, mean = fgpa_mean, sd = fgpa_sigma)
x_bar <- mean(my_sample)
x_bar
[1] 2.910549
x_bar == fgpa_mean
[1] FALSE

CI.eval <- function(pop_mean, sigma, n, estimate, alpha) {
  # The function returns a vector of Booleans (TRUE or FALSE).
  # The function returns TRUE if the confidence interval contains the true population parameter and FALSE if not.

  out <- rep(NA, length(alpha))
  for (i in seq(1, length(alpha))) {
    out[i] <- pop_mean >= estimate - qnorm(alpha[i] / 2, lower.tail = F) * sigma / sqrt(n) &&
      pop_mean <= estimate + qnorm(alpha[i] / 2, lower.tail = F) * sigma / sqrt(n)
  }
  return(out)
}
eval <- CI.eval(
  pop_mean = fgpa_mean,
  sigma = fgpa_sigma,
  n = sample_size,
  estimate = x_bar,
  alpha = c(0.1, 0.05, 0.01)
)
data.frame(eval, row.names = c("90 %", "95 %", "99 %"))
df <- data.frame()
trials <- c(10, 50, 100, 1000, 10000)
n <- 10

for (trial in trials) {
  m <- matrix(NA, nrow = trial, ncol = 3)
  for (i in 1:trial) {
    my_sample <- rnorm(n = n, mean = fgpa_mean, sd = fgpa_sigma)
    x_bar <- mean(my_sample)
    eval <- CI.eval(
      pop_mean = fgpa_mean,
      sigma = fgpa_sigma,
      n = n,
      estimate = x_bar,
      alpha = c(0.1, 0.05, 0.01)
    )
    m[i, ] <- eval
  }
  df <- rbind(df, colSums(m) / trial * 100)
}
row.names(df) <- trials
colnames(df) <- c("90 %", "95 %", "99 %")
df
 90 %   95 %   99 %
10    100.00 100.00 100.00
50     92.00  94.00 100.00
100    86.00  92.00  99.00
1000   90.10  95.10  99.30
10000  89.72  94.86  98.93