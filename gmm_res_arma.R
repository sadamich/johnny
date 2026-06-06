https://cran.r-project.org/web/packages/gmm/refman/gmm.html#residuals

### GEL can deal with endogeneity problems
n = 200
phi<-c(.2,.7)
thet <- 0.2
sd <- .2
set.seed(123)
x <- matrix(arima.sim(n = n, list(order = c(2,0,1), ar = phi, ma = thet, sd = sd)), ncol = 1)

y <- x[7:n]
ym1 <- x[6:(n-1)]
ym2 <- x[5:(n-2)]
H <- cbind(x[4:(n-3)], x[3:(n-4)], x[2:(n-5)], x[1:(n-6)])
g <- y ~ ym1 + ym2
x <- H

res <- gel(g, x, c(0,.3,.6))
summary(res)
Call:gel(g = g, x = x, tet0 = c(0, 0.3, 0.6))
Type of GEL:  EL 
Coefficients:
             Estimate  Std. Error  t value  Pr(>|t|)
(Intercept)  0.00404   0.06824     0.05913  0.95285 
ym1          0.30265   0.14521     2.08425  0.03714 
ym2          0.56867   0.13973     4.06972  0.00005 
Lambdas:
                  Estimate  Std. Error  t value   Pr(>|t|)
Lam((Intercept))  -0.00585   0.00461    -1.26860   0.20458
Lam(h1)            0.09254   0.05330     1.73612   0.08254
Lam(h2)            0.08561   0.03862     2.21675   0.02664
Lam(h3)           -0.14497   0.09421    -1.53878   0.12386
Lam(h4)           -0.06443   0.07631    -0.84429   0.39850
 Over-identifying restrictions tests: degrees of freedom is 2 
         statistics  p-value
LR test  3.97759     0.13686
LM test  3.95528     0.13840
J test   3.95528     0.13840
Convergence code for the coefficients:  0 
Convergence code for the lambdas:  0 

e <- residuals(res)
plot(e)
hist(e)
### ARMA(2,1)
plot(e, type = 'l', main = "Residuals from an ARMA fit using GEL")

### GMM is like GLS for linear models without endogeneity problems
set.seed(345)
n = 200
phi<-c(.2,.7)
thet <- 0
sd <- .2
x <- matrix(arima.sim(n = n, list(order = c(2,0,1), ar = phi, ma = thet, sd = sd)), ncol = 1)
y <- 10 + 5*rnorm(n) + x

res <- gmm(y ~ x, x)
plot(x, residuals(res), main = "Residuals of an estimated model with GMM")
hist(residuals(res))

### https://search.r-project.org/R/refmans/stats/html/arima.sim.html
### Usage: arima.sim(model, n, rand.gen = rnorm, innov = rand.gen(n, ...), ###
### n.start = NA, start.innov = rand.gen(n.start, ...),                    ###
require(graphics)
arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))
plot(arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796)))
### mildly long-tailed
plot(arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5)))
### An ARIMA simulation
ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200)
ts.plot(ts.sim)
