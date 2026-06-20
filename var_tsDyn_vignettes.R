https://cran.r-project.org/web/packages/tsDyn/vignettes/tsDyn.pdf
library(tsDyn)
library(sm)
str(lynx)
Time-Series [1:114] from 1821 to 1934: 269 321 585 871 1475
plot(lynx, type="l")
obj <- llar(log(lynx), m=3)
plot(obj)

obj <- data.frame(obj)
names(obj)
plot(RMSE~eps, data=obj, type="l", log="x")

delta.test(x)


availableModels()


x.new <- predict(obj, n.ahead = )


usual <- linear(lynx, m=3)
adf<- linear(lynx, m=2, type="ADF")


all.equal(deviance(adf), deviance(usual))

all.equal(residuals(usual), residuals(adf))

obj <- setar(x, m=, d=, steps=, thDelay= )

obj <- setar(x, m=, d=, steps=, mTh= )

obj <- setar(x, m=, d=, steps=, thVar= )

obj <- setar(x, m=, d=, steps=, thDelay=, nthresh=2)

obj <- setar(x, m=, d=, steps=, thDelay = , mL =, mH =)

obj <- setar(x, m=, d=, steps=, thDelay = , ML =, MH =)

obj <- nnetTs(x, m=, d=, steps=, size=)

obj <- aar(x, m=, d=, steps=)


x <- log10(lynx)
selectSETAR(x, m=3, mL=1:3, mH=1:3, thDelay=0:2)

summary(lynx)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   39.0   348.2   771.0  1538.0  2566.8  6991.0 
hist(lynx)

plot(lynx)


x <- log10(lynx)

par(mfrow=c(2,1), mar=c(0,0,0,0))
plot(x, ax=F)
box()
plot(x[length(x):1], type="l", ax=F)
box()


par(mfrow=c(2,1), mar=c(2,2,0,0))
autopairs(x, lag=1, type="regression")
autopairs(x, lag=3, type="regression")
hist(x, br=13)


par(mfrow=c(2,1), mar=c(2,4,0,0))
acf(x)
pacf(x)

library(tseriesChaos)
mutual(x)

recurr(x, m=3, d=1, levels=c(0,0.2,1))

lag.plot(x,lags=3,layout=c(1,3))


delta.test(x)

delta.lin.test(x)


mod.ar <- linear(x, m=2)
mod.ar

mod.setar <- setar(x, m=2, mL=2, mH=2, thDelay=1)
mod.setar


mod <- list()
mod[["linear"]] <- linear(x, m=2)
mod[["setar"]] <- setar(x, m=2, thDelay=1)
mod[["lstar"]] <- lstar(x, m=2, thDelay=1)


mod[["nnetTs"]] <- nnetTs(x, m=2, size=3)
mod[["aar"]] <- aar(x, m=2)

sapply(mod, AIC)

sapply(mod, MAPE)


summary(mod[["setar"]])


plot(mod[["setar"]])




set.seed(10)
mod.test <- list()
x.train <- window(x, end=1924)
x.test <- window(x, start=1925)
mod.test[["linear"]] <- linear(x.train, m=2)
mod.test[["setar"]] <- setar(x.train, m=2, thDelay=1)
mod.test[["lstar"]] <- lstar(x.train, m=2, thDelay=1, trace=FALSE, control=list(maxit=1e5))
mod.test[["nnet"]] <- nnetTs(x.train, m=2, size=3, control=list(maxit=1e5))


mod.test[["aar"]] <- aar(x.train, m=2)

frc.test <- lapply(mod.test, predict, n.ahead=10)
plot(x.test,ylim=range(x))
for(i in 1:length(frc.test))
+
lines(frc.test[[i]], lty=i+1, col=i+1)
legend(1925,2.4, lty=1:(length(frc.test)+1), col=1:(length(frc.test)+1), legend=c("observed",names


x.new <- predict(mod[["linear"]], n.ahead=100)
lag.plot(x.new, 1)


x.new <- predict(mod[["setar"]], n.ahead=100)
lag.plot(x.new, 1)

x.new <- predict(mod[["nnetTs"]], n.ahead=100)
lag.plot(x.new, 1)

mod.point <- setar(x, m=10, mL=3, mH=10, thDelay=0, th=3.12)
lag.plot(predict(mod.point, n.ahead=100))


mod.unstable <- setar(x, m=9, mL=9, mH=6, thDelay=4, th=2.61)
lag.plot(predict(mod.unstable, n.ahead=100))

mod.chaos1 <- setar(x, m=5, mL=5, mH=3, thDelay=1, th=2.78)
lag.plot(predict(mod.chaos1, n.ahead=100))


mod.chaos2 <- setar(x, m=5, mL=5, mH=3, thDelay=1, th=2.95)
lag.plot(predict(mod.chaos2, n.ahead=100))


N <- 1000
x.new <- predict(mod[["setar"]], n.ahead=N)
x.new <- x.new + rnorm(N, sd=sd(x.new)/100)
ly <- lyap_k(x.new, m=2, d=1, t=1, k=2, ref=750, s=200, eps=sd(x.new)/10)


plot(ly)

x.new <- predict(mod.chaos2, n.ahead=N)
x.new <- x.new + rnorm(N, sd=sd(x.new)/100)
ly <- lyap_k(x.new, m=5, d=1, t=1, k=2, ref=750, s=200, eps=sd(x.new)/10)

plot(ly)

lyap(ly,start=6,end=70)
