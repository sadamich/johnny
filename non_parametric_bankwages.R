### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 5 Bank wages (p.300)                                         ###
data(morths)
str(morths)

plot(aicplot(deaths~age,weights=n,data=morths,family="binomial",
  alpha=seq(0.2,1.0,by=0.05)))

xm501<- read.csv("xm501.csv",header =TRUE)
str(xm501)
attach(xm501)
library(locfit)
par(mfrow= c(2,2))
### Exhibit 5 7 (a) (p.295)
plot(EDUC,LOGSALARY)
### Local fit 
educ<- EDUC - median(EDUC)
eq<- lm(LOGSALARY~educ)
summary(eq)
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 10.213658   0.014756   692.1   <2e-16 ***
educ         0.095963 
fit<- fitted(eq)
plot(fit, type="l")
### Conditionally parametric term for a Locfit model
### https://cran.r-project.org/web/packages/locfit/refman/locfit.html#cpar
fit_loc <- locfit(LOGSALARY~ cpar(EDUC), data=xm501)
plot(fit_loc)
crit(fit_loc) <- crit(fit_loc,cov=0.99)
plot(fit_loc,band="local")
install.packages("gam")
library(gam)
fit_loc2 <- gam(LOGSALARY ~ lf(EDUC)+GENDER , data=xm501)
op <- par(mfrow=c(2, 1))
plot(fit_loc2)
par(op)

plot.eval(locfit(LOGSALARY ~ lp(EDUC,GENDER, scale=TRUE), data=xm501, ev=lfgrid()))

plot.eval(locfit(LOGSALARY~EDUC+GENDER+MINORITY,data=xm501,scale=0,ev=rbox(cut=0.8)))
plot.eval(locfit(LOGSALARY~EDUC+GENDER,data=xm501,scale=0,ev=rbox(cut=0.3)))



### Exhibit 5 7 (b) (p.295)
f1<- function(x){
result<- 9.062102 + 0.095963*x
return(result)
}
f1(EDUC)
curve(f1,6,22)

hist(EDUC)
median(EDUC)
[1] 12
plot(ecdf(EDUC))
u<- c(1:53 , 425:474)
educ<- EDUC[-u]
logsal<- LOGSALARY[-u]
eq2<- lm(logsal~educ)
fit2<- fitted(eq2)
plot(fit2, type="l")
summary(eq2)
     Estimate Std. Error t value Pr(>|t|)    
(Intercept) 8.815849   0.106600   82.70   <2e-16 ***
educ        0.108950   0.007775   14.01   <2e-16

f2<- function(x){
result<-  8.815849 + 0.108950*x
return(result)
}
f2(EDUC)
curve(f2, 8,22)

### The weight function (p.291)
w<- function(d){
result<- (1-d^3)^3
return(result)
}
w1<- w(0.6)
w1<- rep(w1,474)
eq_w<- lm(LOGSALARY~EDUC, weights=1/w1)
summary(eq_w)
fitw<- fitted(eq_w)
plot(fitw, type="l")

w2<- w(0.9)
w2<- rep(w2,474)
eq_w2<- lm(LOGSALARY~EDUC, weights=1/w2)
summary(eq_w2)
fitw2<- fitted(eq_w2)
plot(fitw2, type="l", add=TRUE, col="red")