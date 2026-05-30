### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm404<- read.csv("xm404.csv", header = TRUE)
attach(xm404)
str(xm404)
detach(xm404)
### Panel 1 (p.264)                                                        ###
eq<- lm(RENDCYCO~RENDMARK)
summary(eq)
sandwich(eq)
 (Intercept)    RENDMARK
(Intercept) 0.117062058 0.004241291
RENDMARK    0.004241291 0.004613880
sqrt(0.004613880)
[1] 0.06792555  (the same with sd of the panel 2 (p.264) 

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

### ML estimation                                                           ###
f<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
sigma<- theta[3]
N<- 240
mu<- beta1+beta2*RENDMARK
-N*0.5*log(2*pi)-N*0.5*log(sigma^2)- 0.5*((RENDCYCO - mu)^2/sigma^2)
}
m<- maxLik(f,start=c(0,0,1))
summary(m)            
 --------------------------------------------
Maximum Likelihood estimation
Newton-Raphson maximisation, 6 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -22287.01 
3  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
[1,] -0.447481   0.023322  -19.19  <2e-16 ***
[2,]  1.171128   0.004846  241.69  <2e-16 ***
[3,] -0.356290   0.001050 -339.41  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### Compare with the panel 1 (p.264) the coeficients are the same (ML=OLS) ###
vcov(m)
 [,1]          [,2]         [,3]
[1,]  5.439291e-04 -1.896482e-05 0.000000e+00
[2,] -1.896482e-05  2.348025e-05 0.000000e+00
[3,]  0.000000e+00  0.000000e+00 1.101909e-06
sqrt(2.348025e-05)
[1] 0.004845642

### 4 3 9 Example (p.245) 
f_t2<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
sigma<- theta[3]
N<- 240
mu<- beta1+beta2*RENDMARK
x<- RENDMARK
e<- RENDCYCO - mu
 N*log(5) -N*0.5*log(sigma^2)- 3*sum(log(1+ e^2/(5*sigma^2)))
}
m2<- maxLik(f_t2,start=c(0,1,1))
summary(m2)
vcov(m2)
 [,1]          [,2]          [,3]
[1,]  0.114318485 -0.0014309283 -0.0011605595
[2,] -0.001430928  0.0047779829 -0.0003659251
[3,] -0.001160559 -0.0003659251  0.0654406510
### Compare with the Panel 4 (p.264)                                       ###
sqrt(0.114318485)
[1] 0.3381102  (for a)
sqrt(0.0047779829)
[1] 0.06912295 (for b)

### Exhibit 4 18 (p.245) and panel 3 (p.264)
Maximum Likelihood estimation
Newton-Raphson maximisation, 8 iterations
Return code 2: successive function values within tolerance limit (tol)
Log-Likelihood: -128.9477 
3  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,] -0.34497    0.33811   -1.02   0.308    
[2,]  1.19641    0.06912   17.31  <2e-16 ***
[3,]  4.49424    0.25581   17.57  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


### BHHH
### 4 3 9 Example (p.245) 
f_b<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
sigma<- theta[3]
N<- 240
mu<- beta1+beta2*RENDMARK
x<- RENDMARK
e<- RENDCYCO - mu
 N*log(5) -N*0.5*log(sigma^2)- 3*sum(log(1+ e^2/(5*sigma^2)))
}

grad_b<-function(beta){
beta1<- beta[1]
beta2<- beta[2]
sigma<- beta[3]
N<- 240
y<- RENDCYCO
x<- RENDMARK
e<- y - (beta1+ beta2*x)
gradient<- matrix(0,N,3)
gradient[ ,1]<- sum(6*e/(5*sigma^2 +e^2))
gradient[ ,2]<- sum(6*e*x/(5*sigma^2 +e^2))
gradient[ ,3] <- -N/2*sigma^2 +3/sigma^2*(sum(6*e/(5*sigma^2 +e^2)))
gradient
}


mb<- maxBHHH(f_b,grad = grad_b,start=c(0,0,1))
summary(mb)





### QML (p.263)????
g<- function(beta,y,x,e){
y<- RENDCYCO
x<- RENDMARK
beta1<- beta[1]
beta2<- beta[2]
e<- y - (beta1+ beta2*x)
m1<- (6*e/(5*4.49424 +e^2))
m2<-  (6*e*x/(5*4.49424 +e^2))
f<- cbind(m1,m2)
return(f)
}

Dg<- function(beta,y,x,e){
y<- RENDCYCO
x<- RENDMARK
beta1<- beta[1]
beta2<- beta[2]
e<- y - (beta1+ beta2*x)
H<- matrix(c(-12*e/(5*4.49424 +e^2)+6*e/(5*4.49424 +e^2)^2,
             -12*e*x/(5*4.49424 +e^2)+6*e*x/(5*4.49424 +e^2)^2,
             -12*e*x/(5*4.49424 +e^2)+6*e*x/(5*4.49424 +e^2)^2,
             -12*e*x^2/(5*4.49424 +e^2)+6*e*x^2/(5*4.49424 +e^2)^2,
           nrow=2,ncol=2))
return(H)
}
eq_gmm<- gmm(g,x=RENDMARK,c(beta1=0,beta2=0),grad =Dg)
summary(eq_gmm)


y<- RENDCYCO
x<- RENDMARK
beta1<- -0.34497
beta2<- 1.19641 
e<- y - (beta1+ beta2*x)
m1<- (6*e/(5*4.49424 +e^2))
m2<-  (6*e*x/(5*4.49424 +e^2))
f<- cbind(m1,m2)
j<- f%*%t(f)

y<- RENDCYCO
x<- RENDMARK
beta1<- 0.34497
beta2<- 1.19641 
e<- y - (beta1+ beta2*x)
H<- matrix(c(-12*e/(5*4.49424 +e^2)+6*e/(5*4.49424 +e^2)^2,
             -12*e*x/(5*4.49424 +e^2)+6*e*x/(5*4.49424 +e^2)^2,
             -12*e*x/(5*4.49424 +e^2)+6*e*x/(5*4.49424 +e^2)^2,
             -12*e*x^2/(5*4.49424 +e^2)+6*e*x^2/(5*4.49424 +e^2)^2,
           nrow=2,ncol=2))
j_1<- 1/j
H_t<- t(H)
H%*%j_1%*%H_t


### Exhibit 4 18 f (p.245)                                                ###
res_ml<- RENDCYCO -(-0.34497 +1.19641*RENDMARK)
plot(res_ml, type="l")
hist(res_ml)
summary(res_ml)
 Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-20.60286  -3.62816   0.06664  -0.12296   3.30268  14.84975 


### LR test (p. 246)                                                       ###

2*(-747.16+750.54) 
[1] 6.76
1- pchisq(6.76,1)
[1] 0.009322376
### H0 (e is normal distributed) is rejected.                              ###

### 4 4 6 (p.262) the GMM standard errors    
(1.171128 -1)/ 0.075386
[1] 2.270024
2*(1-pt(2.270024,238))
[1] 0.02410117       (P value:OLS)

(1.171128 -1)/0.067926
[1] 2.51933
2*(1 -pt(2.51933,238))
[1] 0.01241407       (P value:GMM OLS)

(1.196406 - 1)/0.073841
[1] 2.65985
2*(1- pt(2.65985,238))
[1] 0.008348761      (P value:ML)

(1.196406 - 1)/0.066475
[1] 2.954584   
2*(1 -pt(2.954584,238))  
[1] 0.003445095      (P value:GMM ML)

(-0.344971)/0.334173  
[1] -1.032313  
2*pt(-1.032313,238)  
[1] 0.3029734     


g1<- function(beta,x){
beta1<- beta[1]
beta2<- beta[2]
sigma<-beta[3]
N<- 240
m1<- sum(6*(RENDCYCO - beta[1]-beta[2]*RENDMARK)/
        (5*sigma^2+(RENDCYCO - beta[1]-beta[2]*RENDMARK)^2))
m2<- sum(RENDMARK*(RENDCYCO - beta[1]-beta[2]*RENDMARK)/
        (5*sigma^2+(RENDCYCO - beta[1]-beta[2]*RENDMARK)^2))
m3<- -N/2*sigma^2+ 3/sigma^2*sum((RENDCYCO - beta[1]-beta[2]*RENDMARK)^2/
        (5*sigma^2+(RENDCYCO - beta[1]-beta[2]*RENDMARK)^2))
f<- cbind(m1,m2,m3)
return(f)
}
eq_gmm<- gmm(g1,x=RENDMARK,c(beta1=0,beta2=1,sigma=1))
summary(eq_gmm)
vcov(eq_gmm)
       