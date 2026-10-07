library(tseries)

n <- 1000
x <- runif(1000, -1, 1)  # Non-linear in ``mean'' regression 
y <- x^2 - x^3 + 0.1*rnorm(x)
terasvirta.test(x, y)

## Is the polynomial of order 2 misspecified?
terasvirta.test(cbind(x,x^2,x^3), y)

## Generate time series which is nonlinear in ``mean''
x[1] <- 0.0
for(i in (2:n)) {
  x[i] <- 0.4*x[i-1] + tanh(x[i-1]) + rnorm(1, sd=0.5)
}
x <- as.ts(x)
plot(x)
terasvirta.test(x)

Details
The null is the hypotheses of linearity in “mean”. This test uses a Taylor 
series expansion of the activation function to arrive at a suitable test 
statistic. If type equals "F", then the F-statistic instead of the Chi-Squared
 statistic is used in analogy to the classical linear regression.

