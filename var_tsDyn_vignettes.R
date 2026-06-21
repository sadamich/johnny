https://cran.r-project.org/web/packages/tsDyn/vignettes/tsDyn.pdf
library(tsDyn)
library(sm)
### 2 Explorative analysis: 2 1 bivariate and trivariate relation
data(zeroyld)
str(zeroyld)
'data.frame':   482 obs. of  2 variables:
 $ short.run: num  2.18 2.25 2.31 2.4 2.56 ...
 $ long.run : num  1.57 1.54 1.76 1.77 1.81 .
attach(zeroyld)
x<- short.run
str(x)
num [1:482] 2.18 2.25 2.31 2.4 2.56 
y<- long.run
str(y)
num [1:482] 1.57 1.54 1.76 1.77 1.81 ...
plot(x, type="l")
plot(y, type ="l")
autopairs(x, lag=1,type="regression")
autopairs(
  x,
  lag = 1,
  h,
  type = c("levels", "persp", "image", "lines", "points", "regression")
)

### 2 2 Linearity: the locally linear regression fit                     ###
str(lynx)
Time-Series [1:114] from 1821 to 1934: 269 321 585 871 1475
plot(lynx, type="l")
obj <- llar(log(lynx), m=3)
plot(obj)
obj <- data.frame(obj)
names(obj)
plot(RMSE~eps, data=obj, type="l", log="x")
### 2 3 Test
delta.test(x)
 eps     ### x is shurt.run
m   1.524 3.049 4.573 6.098
  2  0.02  0.02  0.02  0.02
  3  0.02  0.02  0.02  0.02

### 3 Nonlinear autoregressive time series models
availableModels()
x.new <- predict(obj, n.ahead = )

### 3 1 linear model 
usual <- linear(lynx, m=3)
plot(usual, type="l")
summary(usual)
usual_f<- predict(usual, n.ahead =5 )
plot(usual_f, type="l")
adf<- linear(lynx, m=2, type="ADF")
summary(adf)
Non linear autoregressive model
AR model
Coefficients:
       const        phi.1       Dphi.1       Dphi.2 
733.62986445  -0.46726808   0.60329908   0.02591132 
Residuals:
       Min         1Q     Median         3Q        Max 
-2561.5930  -466.2500  -193.9872   438.4594  3177.3749 

Fit:
residuals variance = 761560,  AIC = 1552, MAPE = 636.6%
Coefficient(s):
         Estimate  Std. Error  t value  Pr(>|t|)    
const  733.629864  138.836458   5.2841 6.711e-07 ***
phi.1   -0.467268    0.072063  -6.4842 2.825e-09 ***
Dphi.1   0.603299    0.077512   7.7833 4.748e-12 ***
Dphi.2   0.025911    0.095596   0.2711    0.7869    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
all.equal(deviance(adf), deviance(usual))
all.equal(residuals(usual), residuals(adf))

### 3.2 SETAR models
obj <- setar(x, m=, d=, steps=, thDelay= )
obj <- setar(x, m=, d=, steps=, mTh= )
obj <- setar(x, m=, d=, steps=, thVar= )
obj <- setar(x, m=, d=, steps=, thDelay=, nthresh=2)
obj <- setar(x, m=, d=, steps=, thDelay = , mL =, mH =)
obj <- setar(x, m=, d=, steps=, thDelay = , ML =, MH =)
### 3.3 LSTAR models
### 3.4 Neural Network models
obj <- nnetTs(x, m=, d=, steps=, size=)
mod.nnet <- nnetTs(log(lynx), m=2, size=3)
mod.nnet
plot(mod.nnet,type ="l")
### 3.5 Additive Autoregressive models
obj <- aar(x, m=, d=, steps=)
#fit an AAR model:
mod <- aar(log(lynx), m=3)
#Summary informations:
summary(mod)
#Diagnostic plots:
plot(mod)

### 3.6 Model selection
x <- log10(lynx)
selectSETAR(x, m=3, mL=1:3, mH=1:3, thDelay=0:2)


### 4 Casestudy
summary(lynx)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   39.0   348.2   771.0  1538.0  2566.8  6991.0 
hist(lynx)

plot(lynx)
### 4.1 Explorative analysis
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

### 4.2 Model selection
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


### 4.3 Out-of-sample forecasting
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

### 4.4 Inspecting model skeleton
x.new <- predict(mod[["linear"]], n.ahead=100)
lag.plot(x.new, 1)
x.new <- predict(mod[["setar"]], n.ahead=100)
lag.plot(x.new, 1)
x.new <- predict(mod[["nnetTs"]], n.ahead=100)
lag.plot(x.new, 1)

### 5 Sensitivity on initial conditions
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
