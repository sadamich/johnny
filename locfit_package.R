
library(locfit)
data(ethanol, package="locfit")
str(ethanol)
'data.frame':   88 obs. of  3 variables:
 $ NOx: num  3.74 2.29 1.5 2.88 0.76 ...
 $ C  : num  12 12 12 12 12 9 9 9 12 12 ...
 $ E  : num  0.907 0.761 1.108 1.016 1.189 ...
# fit a conditionally parametric model
fit <- locfit(NOx ~ lp(E, C, style=c("n","cpar")), data=ethanol)
plot(fit)
# one way to force a parametric fit with locfit
fit <- locfit(NOx ~ cpar(E), data=ethanol)
plot(fit)

### density estimaiton
data(geyser)
str(geyser)
z<- density.lf(geyser, window="tria")
plot(z)
# the same result with density, except less precision.
density(geyser, window="tria")
Call:
        density.default(x = geyser, window = "tria")

Data: geyser (107 obs.);        Bandwidth 'bw' = 0.3677

       x                y          
 Min.   :0.5668   Min.   :0.00000  
 1st Qu.:1.9334   1st Qu.:0.06031  
 Median :3.3000   Median :0.15581  
 Mean   :3.3000   Mean   :0.18259  
 3rd Qu.:4.6666   3rd Qu.:0.25655  
 Max.   :6.0332   Max.   :0.48314  

### effective Kernel
hatmatrix(formula, dc=TRUE, ...)


### Critical Values for Simultaneous Confidence Bands

# compute and plot simultaneous confidence bands
data(ethanol)
fit <- locfit(NOx~E,data=ethanol)
crit(fit) <- kappa0(NOx~E,data=ethanol)
plot(fit,crit=crit,band="local")


### Bandwidth selectors for kernel density estimation
kdeb(x, h0 = 0.01 * sd, h1 = sd, meth = c("AIC", "LCV", "LSCV", "BCV", 
  "SJPI", "GKK"), kern = "gauss", gf = 2.5)


kdeb(E, h0 = 0.3 , h1 = 0.9, meth = c("LCV"), kern = "gauss", gf = 2.5)



data(ethanol)
plot(lcvplot(NOx~E,data=ethanol,alpha=seq(0.2,1.0,by=0.05)))


### Locfit - grid evaluation structure

data(ethanol, package="locfit")
plot.eval(locfit(NOx ~ lp(E, C, scale=TRUE), data=ethanol, ev=lfgrid()))

### Robust
data(ethanol, package="locfit")
plot.eval(locfit(NOx~E+C,data=ethanol,scale=0,ev=rbox(cut=0.8)))
plot.eval(locfit(NOx~E+C,data=ethanol,scale=0,ev=rbox(cut=0.3)))


### Sheather-Jones Plug-in bandwidth criterion
# Fig 10.2 (S-J parts) from Loader (1999).
data(geyser, package="locfit")
gf <- 2.5
a <- seq(0.05, 0.7, length=100)
z <- sjpi(geyser, a)

# the plug-in curve. Multiplying by gf=2.5 corresponds to Locfit's standard
# scaling for the Gaussian kernel.
plot(gf*z[, 2], gf*z[, 1], type = "l", xlab = "Pilot Bandwidth k", ylab
     = "Bandwidth h")
# Add the assumed curve.
lines(gf * z[, 3], gf * z[, 1], lty = 2)
legend(gf*0.05, gf*0.4, lty = 1:2, legend = c("Plug-in", "SJ assumed"))

