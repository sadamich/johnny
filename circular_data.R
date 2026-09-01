https://cran.r-project.org/web/packages/circular/refman/circular.html#Circular+20Uniform
install.packages("circular")
library(circular)
data1 <- rcircularuniform(100, control.circular=list(units="degrees"))
plot(data1)

curve.circular(dcircularuniform, join=TRUE, xlim=c(-1.2, 1.2), 
  ylim=c(-1.2, 1.2), main="Density of a Circular Uniform Distribution")


### Inverse of A1
#Generate data from a von Mises distribution
data <- rvonmises(n=50, mu=circular(pi), kappa=4)
#Estimate the concentration parameter
s <- sum(sin(data))
c <- sum(cos(data))
mean.dir <- atan2(s, c)
mean.dir
[1] 3.030896
kappa <- A1inv(mean(cos(data - mean.dir)))
kappa
[1] 5.2449


### Cardioid Density Function
 set.seed(1234) 
  resrad <- rcardioid(n=10)
  set.seed(1234)
  resdeg <- rcardioid(n=10, control.circular=list(units="radians", zero=pi))  
  max(abs(resrad - conversion.circular(resdeg, zero=0)))
plot(resdeg)


### Add Arrows to a Circular Plot
plot(rvonmises(10, circular(0), kappa=1))
  arrows.circular(rvonmises(10, circular(0), kappa=1))
  arrows.circular(rvonmises(10, circular(0), kappa=1), y=runif(10), col=2)
  arrows.circular(rvonmises(10, circular(0), kappa=1), y=runif(10), 
    x0=runif(10, -1, 1), y0=runif(10, -1, 1), col=3)


### Asymmetric Triangular Density Function
ff <- function(x) dasytriangular(x, rho=0.3)
curve.circular(ff, shrink=1.2, join=TRUE)


### Angles between a vector and the x-axis
set.seed(1234)
x <- cbind(rnorm(20), rnorm(20))
y <- coord2rad(x)
y
lar Data: 
Type = angles 
Units = radians 
Template = none 
Modulo = 2pi 
Zero = 0 
Rotation = counter 
 [1] 3.030960 5.226972 5.897308 2.948115 5.266362 5.048562 2.356181 4.221913
 [9] 3.168406 3.952129 1.979345 3.586151 3.882050 4.840281 5.244662 4.618215
[17] 4.482142 4.115559 3.479633 6.092673
plot(y)


### Draw Function Plots in a Circle
ff <- function(x) sqrt(x)/20
curve.circular(ff)
curve.circular(ff, to=6*pi, join=FALSE, nosort=TRUE, n=1001, modulo="asis",
  shrink=1.2)

plot.function.circular(function(x) dvonmises(x, circular(0), 10), xlim=c(-1, 2.2))


### B.18 Wind direction and ozone concentration
data(fisherB18)
data(fisherB18c)
par(mfcol=c(1,3))
plot(fisherB18c$theta, xlab=expression(theta))
boxplot(fisherB18c$x, xlab="x")
plot(c(fisherB18$x, fisherB18$x), c(fisherB18$theta,
  fisherB18$theta+360), xlab="x", ylab=expression(theta))


### Triangular Density Function
data1 <- rtriangular(100, 0.3, control.circular=list(units="degrees"))
plot(data1)
ff <- function(x) dtriangular(x, rho=0.3)
curve.circular(ff, shrink=1.2, join=TRUE)

### Trigonometric Moments
x <- rvonmises(100, circular(0), 5)
trigonometric.moment(x, control.circular=list(units="degrees"))


