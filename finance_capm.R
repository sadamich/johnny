### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm404<- read.csv("xm404.csv", header = TRUE)
attach(xm404)
### Package gmm ###
install.packages("gmm")
library("gmm")
eq_gmm<- gmm(RENDCYCO~RENDMARK, x=RENDMARK)
summary(eq_gmm)
### 4 4 6 Illustration Stock market returns (p.262)                        ###
### Panel 2 (p.264) Call:gmm(g = RENDCYCO ~ RENDMARK, x = RENDMARK)        ###
Method:  twoStep 
Kernel:  Quadratic Spectral
Coefficients:
             Estimate     Std. Error   t value      Pr(>|t|)   
(Intercept)  -4.4748e-01   3.3932e-01  -1.3188e+00   1.8724e-01
RENDMARK      1.1711e+00   6.9309e-02   1.6897e+01   4.7226e-64
J-Test: degrees of freedom is 0 
                J-test                P-value             
Test E(g)=0:    8.07282570225798e-26  *******

### Compare with the Panel 3 (p.264)                                       ###
const<- rep(1, 240)
set.seed(74)
eps<- rt(240, 5)
X<- cbind(const, RENDMARK, eps)
y<- RENDCYCO
negSSE <- function(beta) {
e <- y- X %*% beta
-crossprod(e)
}
m <- maxLik(negSSE, start=c(0,0,1))
summary(m, eigentol=1e-15)

Maximum Likelihood estimation
Newton-Raphson maximisation, 3 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -7310.359 
3  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,] -0.44857    0.04634  -9.681  <2e-16 ***
[2,]  1.17137    0.00962 121.766  <2e-16 ***
[3,] -0.06745    0.03884  -1.736  0.0825 .  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


             
 
