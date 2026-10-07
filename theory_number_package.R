library(numbers)

#-- Continued fraction of sqrt(2) is [1; 2, 2, 2, ...] -----------------
b0 <- 1; b <- rep(2, 20)                # sqrt(2)
cf2num(b0, b)                           # 1.414213562, error = eps()

#-- Approximate an analytic function -----------------------------------
# tan(x) = 0 + x / (1 + (-x^2) / 3 + (-x^2) / (5 + ...))
x  <- 0.5                           # tan(0.5) = 0.546302489844
n  <- 10                            # CF of length 20
b0 <- 0                             # b0 = 0
b  <- seq(1, by=2, length=n)        # b = c(1, 3, 5, ...)
a  <- c(x, rep(-x^2, times=n-1))    # a = c(x, -x^2, -x^2, ...)

cf2num(b0, b, a)                # 0.546302489844, error: eps()

## Not run: 
#-- Continued fraction of 1/(exp(0.5)-1) -------------------------------
library(Rmpfr)
e0 = 1/(exp(1/mpfr(2, precBits=128)) - 1)  #=> 1.54149408253679828413...
x0 <- num2cf(e0, nterms=20)
## [1]  1  1  1  5  1  1  9  1  1 13  1  1 17  1  1 21  1  1 25  1  1
https://cran.r-project.org/web/packages/numbers/refman/numbers.html#cf2num
# Determine e0 from its continued fraction
b0 <- x0[1]; b <- x0[2:19]
cf2num(b0, b)                       # 1.541494082536798, error = eps()

#-- pi as arctan(1.0) --------------------------------------------------
n = 24
b0 = 0
b = seq(1, by=2, length=n)          # b = c(1, 3, 5, 7, ...)
a = c(1, seq(1,by=1, length=n-1)^2) # a = c(1, 1, 4, 9, ...)
4 * cf2num(b0, b, a) - pi           # error: 0

#-- Leibniz-Wallis continued fraction for pi ---------------------------
# 4/pi = 1 + 1^2/(2 + 3^2 / (2 + 5^2 / 2 + ...))
pi4 = 4 / pi                        # 1.27323954474

n  = 20
b0 = 1
b  = rep(2, times=n)
a  = seq(1, by = 2, length=n)^2
cf2num(b0, b, a)                    # NA, i.e. convergents overflow
cf2num(b0, b, a, scaled = TRUE)
# 1.273272 with estimated precision 0.004032851 
# [1] 1.273272                      # actual error: 3.3e-05
  


eulersPhi(9973)  == 9973 - 1                       # for prime numbers
eulersPhi(3^10)  == 3^9 * (3 - 1)                  # for prime powers
eulersPhi(12*35) == eulersPhi(12) * eulersPhi(35)  # TRUE if coprime

## Not run: 
x <- 1:100; y <- sapply(x, eulersPhi)
plot(1:100, y, type="l", col="blue",
               xlab="n", ylab="phi(n)", main="Euler's totient function")
points(1:100, y, col="blue", pch=20)
grid()