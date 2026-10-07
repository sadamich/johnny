library(tseries)

n <- 500  # Generate AR(1) process
a <- 0.6
e <- rnorm(n+100)  
x <- double(n+100)
x[1] <- rnorm(1)
for(i in 2:(n+100)) {
  x[i] <- a * x[i-1] + e[i]
}
x <- ts(x[-(1:100)])
tsbootstrap(x, nb=500, statistic=mean)
Call:
tsbootstrap(x = x, nb = 500, statistic = mean)
Resampled Statistic(s):
  original       bias std. error 
 0.0959129 -0.0007197  0.0811583 

# Asymptotic formula for the std. error of the mean
sqrt(1/(n*(1-a)^2))

acflag1 <- function(x)
{
  xo <- c(x[,1], x[1,2])
  xm <- mean(xo)
  return(mean((x[,1]-xm)*(x[,2]-xm))/mean((xo-xm)^2))
}
tsbootstrap(x, nb=500, statistic=acflag1, m=2)
Call:
tsbootstrap(x = x, nb = 500, statistic = acflag1, m = 2)

Resampled Statistic(s):
  original       bias std. error 
  0.529672  -0.006357   0.043818 

# Asymptotic formula for the std. error of the acf at lag one
sqrt(((1+a^2)-2*a^2)/n)