### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 4 6 Coffee sales                                               ###
xm402 <- read.csv("xm402.csv", header = TRUE)
str(xm402)
attach(xm402)

f_cof <- function(theta) {
beta_1<- theta[1]
beta_2<- theta[2]
beta_3<- theta[3]
LOGQ1-(beta_1 + (beta_2/beta_3) * (D1^beta_3 -1))
}
### maximize wrt. both parameters                                          ###
free2 <- maxNR(f_cof, start=c(0,0,0))
summary(free2)????


### The Newton Raphson iterations (4 15) (p.210)
### https://cran.r-project.org/web/packages/maxLik/refman/maxLik.html#maxNR
theta_h_1<- function(theta){
result<- theta - (H_h)^(-1)*G_h
return(result)
}
### Marquardt
theta_h_2<- function(theta){
result<- theta - (H_h +cI)^(-1)*G_h
return(result)
}
library(maxLik)
f <- function(a) exp(-a[1]^2 - a[2]^2)
### maximize wrt. both parameters                                          ###
free <- maxNR(f, start=1:2) 
summary(free)

f2 <- function(theta) {
a<- theta[1]
b<- theta[2]
exp(-a^2 - b^2)
}
### maximize wrt. both parameters                                          ###
free2 <- maxNR(f2, start=1:2) 
summary(free2)

f3<- function(x){
-1/2*x^2
}
free3<- maxNR(f3, start=0)
summary(free3)

f4<- function(x){
-1/2*x^2 - 100
}
free4<- maxNR(f4, start=0)
summary(free4)


f5<- function(x){
-1/2*(x - 100)^2 
}
free5<- maxNR(f5, start=0)
summary(free5)


f6<- function(x){
(1 + sin(x))*cos(x)
}
free6<- maxNR(f6, start=0)
summary(free6)
--------------------------------------------
Newton-Raphson maximisation 
Number of iterations: 3 
Return code: 1 
gradient close to zero (gradtol) 
Function value: 1.299038 
Estimates:
      estimate      gradient
[1,] 0.5235988 -2.753353e-08
pi/6
[1] 0.5235988
3*sqrt(3)/4
[1] 1.299038

f7<- function(x){
-x^3+3*x^2
}
free7<- maxNR(f7, start=0)
summary(free7)
Newton-Raphson maximisation 
Number of iterations: 1 
Return code: 1 
gradient close to zero (gradtol) 
Function value: 7.324225e-14 
Estimates:
          estimate      gradient
[1,] -1.562501e-07 -9.375007e-07

free7<- maxNR(f7, start=1.5)
summary(free7)
Newton-Raphson maximisation 
Number of iterations: 4 
Return code: 1 
gradient close to zero (gradtol) 
Function value: 4 
Estimates:
     estimate     gradient
[1,]        2 6.554757e-07


f8<- function(x){
x^3-3*x^2+4
}
free8<- maxNR(f8, start=0)
summary(free8)
Newton-Raphson maximisation 
Number of iterations: 1 
Return code: 1 
gradient close to zero (gradtol) 
Function value: 4 
Estimates:
     estimate gradient
[1,]        0        0
free8<- maxNR(f8, start=1.9)
summary(free8)

### The factorization: z^n -1 = pi*pnorm(z)
pi*pnorm(1)
### (x-1)(x-2)(2x+1)                                                   ###
f9<- function(x){
2*x^3-5*x^2+x+2
}
free9<- maxNR(f9, start=1.5)
summary(free9)
Newton-Raphson maximisation 
Number of iterations: 3 
Return code: 1 
gradient close to zero (gradtol) 
Function value: 2.052205 
Estimates:
      estimate      gradient
[1,] 0.1068502 -2.664535e-09



### R (Seite   )
x_neu <- 5
x_alt <- 0
while(abs(x_alt - x_neu) >= 1e-10) {
x_alt <- x_neu
x_neu <- x_alt - x_alt/3
}
c(ergebnis = x_neu, differenz = abs(x_alt - x_neu))
### x_neu = x_alt - (H)^(-1)*G                                             ###