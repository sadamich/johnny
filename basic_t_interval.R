### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm101<- read.csv("xm101.csv", header=TRUE)
str(xm101)
attach(xm101)
https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Inferential-Statistics/t-Distribution/index.html
lower_90 <- qt(0.05, df = 12, lower.tail = TRUE)
upper_90 <- qt(0.05, df = 12, lower.tail = FALSE)
paste("The lower and upper limits of the interval that covers an area of 90 % around the mean are given by t-values (df=12) of", round(lower_90, 2), "and", round(upper_90, 2), ", respectively.")

lower_95 <- qt(0.025, df = 12, lower.tail = TRUE)
upper_95 <- qt(0.025, df = 12, lower.tail = FALSE)
paste("The lower and upper limits of the interval that covers an area of 95 % around the mean are given by t-values (df=12) of", round(lower_95, 2), "and", round(upper_95, 2), ", respectively.")

lower_99 <- qt(0.005, df = 12, lower.tail = TRUE)
upper_99 <- qt(0.005, df = 12, lower.tail = FALSE)
paste("The lower and upper limits of the interval that covers an area of 99 % around the mean are given by t-values (df=12) of", round(lower_99, 2), "and", round(upper_99, 2), ", respectively.")

### The Example of FGPA 
hist(FGPA[FEM==1], breaks ="FD")
fgpa_mean<- mean(FGPA[FEM==1])
fgpa_mean
[1] 2.894831
fgpa_sigma<- sd(FGPA[FEM==1])
fgpa_sigma
[1] 0.4719426

set.seed(335)
sample_size <- 6
df <- sample_size - 1

my_sample_f <- rnorm(n = sample_size, mean = fgpa_mean, sd = fgpa_sigma)
x_bar_f <- mean(my_sample_f)
x_bar_f
s_f <- sd(my_sample_f)
s_f
x_bar_f == fgpa_mean
[1] FALSE
s_f == fgpa_sigma
## [1] FALSE
s_f < fgpa_sigma
[1] TRUE

CI.eval.t <- function(pop_mean, s, n, estimate, alpha) {
  # The function returns a vector of Booleans (TRUE or FALSE).
  # The function returns TRUE if the confidence interval contains the true population parameter and FALSE if not.
  out <- rep(NA, length(alpha))
  for (i in seq(1, length(alpha))) {
    out[i] <- pop_mean >= estimate - qt(alpha[i] / 2, df = n - 1, lower.tail = F) * s / sqrt(n) &&
      pop_mean <= estimate + qt(alpha[i] / 2, df = n - 1, lower.tail = F) * s / sqrt(n)
  }
  return(out)
}
eval <- CI.eval.t(
  pop_mean = fgpa_mean,
  s = s_f,
  n = sample_size,
  estimate = x_bar_f,
  alpha = c(0.1, 0.05, 0.01)
)
data.frame(eval, row.names = c("90 %", "95 %", "99 %"))

set.seed(335)
df <- data.frame()
trials <- c(10, 50, 100, 1000, 10000)
n <- 6
for (trial in trials) {
  m <- matrix(NA, nrow = trial, ncol = 3)
  for (i in 1:trial) {
    my_sample <- rnorm(n = n, mean = fgpa_mean, sd = fgpa_sigma)
    x_bar <- mean(my_sample)
    s <- sd(my_sample)
    eval <- CI.eval.t(
      pop_mean = fgpa_mean,
      s = s,
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
   90 %  95 %   99 %
10    70.00 80.00 100.00
50    88.00 94.00  98.00
100   86.00 93.00 100.00
1000  90.50 95.00  98.90
10000 89.99 95.07  99.08
