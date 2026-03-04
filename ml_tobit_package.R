### https://cran.r-project.org/web/packages/sampleSelection/vignettes/selection.pdf ###
install.pachages("sampleSelection")
library(sampleSelection)

set.seed(0)
install.packages("mvtnorm")
library("mvtnorm")
eps <- rmvnorm(500, c(0,0), matrix(c(1,-0.7,-0.7,1), 2, 2))
xs <- runif(500)
ys <- xs + eps[,1] > 0
xo <- runif(500)
yoX <- xo + eps[,2]
yo <- yoX*(ys > 0)

summary( selection(ys~xs, yo ~xo))

Tobit 2 model (sample selection model)
Maximum Likelihood estimation
Newton-Raphson maximisation, 5 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -712.3163 
500 observations (172 censored and 328 observed)
6 free parameters (df = 494)
Probit selection equation:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -0.2228     0.1081  -2.061   0.0399 *  
xs            1.3377     0.2014   6.642 8.18e-11 ***
Outcome equation:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.0002265  0.1294178  -0.002    0.999    
xo           0.7299070  0.1635925   4.462 1.01e-05 ***
   Error terms:
      Estimate Std. Error t value Pr(>|t|)    
sigma   0.9190     0.0574  16.009  < 2e-16 ***
rho    -0.5392     0.1521  -3.544 0.000431 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1

yoX<-xs+ eps[,2]
yo<-yoX*(ys > 0)
summary(selection(ys~ xs,yo~ xs))

Tobit 2 model (sample selection model)
Maximum Likelihood estimation
Newton-Raphson maximisation, 14 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -712.8298 
500 observations (172 censored and 328 observed)
6 free parameters (df = 494)
Probit selection equation:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -0.1984     0.1114  -1.781   0.0756 .  
xs            1.2907     0.2085   6.191 1.25e-09 ***
Outcome equation:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  -0.5499     0.5644  -0.974  0.33038   
xs            1.3987     0.4482   3.120  0.00191 **
   Error terms:
      Estimate Std. Error t value Pr(>|t|)    
sigma  0.85091    0.05352  15.899   <2e-16 ***
rho   -0.13226    0.72684  -0.182    0.856    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ 


xs <- runif(500,-5, 5)
ys <- xs + eps[,1] > 0
yoX <- xs + eps[,2]
yo <- yoX*(ys > 0)
summary( selection(ys ~ xs, yo ~ xs))
Tobit 2 model (sample selection model)
Maximum Likelihood estimation
Newton-Raphson maximisation, 4 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -440.883 
500 observations (247 censored and 253 observed)
6 free parameters (df = 494)
Probit selection equation:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.07401    0.10527  -0.703    0.482    
xs           0.98825    0.08860  11.154   <2e-16 ***
Outcome equation:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.19333    0.14462   1.337    0.182    
xs           0.94646    0.04575  20.689   <2e-16 ***
   Error terms:
      Estimate Std. Error t value Pr(>|t|)    
sigma  1.04038    0.04928  21.110  < 2e-16 ***
rho   -0.77495    0.09433  -8.216 1.87e-15 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### Switching Model                                                       ###

set.seed(0)
vc<-diag(3)
vc[lower.tri(vc)] <-c(0.9, 0.5,0.1)
vc[upper.tri(vc)] <-vc[lower.tri(vc)]
eps<-rmvnorm(500, c(0,0,0),vc)
xs<-runif(500)
ys<-xs+ eps[,1] > 0
xo1<-runif(500)
yo1<-xo1+ eps[,2]
xo2<-runif(500)
yo2<-xo2+ eps[,3]
summary(selection(ys~xs, list(yo1 ~ xo1, yo2 ~ xo2)))
Tobit 5 model (switching regression model)
Maximum Likelihood estimation
Newton-Raphson maximisation, 9 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -889.7797 
500 observations: 147 selection 1 (FALSE) and 353 selection 2 (TRUE)
10 free parameters (df = 490)
Probit selection equation:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.07930    0.09677   0.819    0.413    
xs           0.94218    0.15025   6.271  7.9e-10 ***
Outcome equation 1:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.05085    0.14926  -0.341    0.733    
xo1          1.27859    0.14755   8.666   <2e-16 ***
Outcome equation 2:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.08099    0.16579   0.488    0.625    
xo2          0.92757    0.17641   5.258 2.18e-07 ***
   Error terms:
       Estimate Std. Error t value Pr(>|t|)    
sigma1  1.01763    0.09289  10.955   <2e-16 ***
sigma2  0.99424    0.07973  12.469   <2e-16 ***
rho1    0.95305    0.02199  43.347   <2e-16 ***
rho2    0.54042    0.23372   2.312   0.0212 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 


set.seed(5)
eps <- rmvnorm(1000, rep(0, 3), vc)
eps <- eps^2- 1
xs <- runif(1000,-1, 0)
 ys <- xs + eps[,1] > 0
xo1 <- runif(1000)
yo1 <- xo1 + eps[,2]
xo2 <- runif(1000)
yo2 <- xo2 + eps[,3]
summary(selection(ys~xs, list(yo1 ~ xo1, yo2 ~ xo2), iterlim=20))
Tobit 5 model (switching regression model)
Maximum Likelihood estimation
Newton-Raphson maximisation, 4 iterations
Return code 3: Last step could not find a value above the current.
Boundary of parameter space?  
Consider switching to a more robust optimisation method temporarily.
Log-Likelihood: -1665.936 
1000 observations: 760 selection 1 (FALSE) and 240 selection 2 (TRUE)
10 free parameters (df = 990)
Probit selection equation:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.53698    0.05808  -9.245  < 2e-16 ***
xs           0.31268    0.09395   3.328 0.000906 ***
Outcome equation 1:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.70679    0.03573  -19.78   <2e-16 ***
xo1          0.91603    0.05626   16.28   <2e-16 ***
Outcome equation 2:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.1446        NaN     NaN      NaN  
xo2           1.1196     0.5014   2.233   0.0258 *
   Error terms:
       Estimate Std. Error t value Pr(>|t|)    
sigma1  0.67770    0.01760   38.50   <2e-16 ***
sigma2  2.31432    0.07615   30.39   <2e-16 ***
rho1   -0.97137        NaN     NaN      NaN    
rho2    0.17039        NaN     NaN      NaN    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
Warning messages:


set.seed(6)
xs <- runif(1000,-1, 1)
ys <- xs + eps[,1] > 0
yo1 <- xs + eps[,2]
yo2 <- xs + eps[,3]
summary(tmp <- selection(ys~xs, list(yo1 ~ xs, yo2 ~ xs), iterlim=20))
Tobit 5 model (switching regression model)
Maximum Likelihood estimation
Newton-Raphson maximisation, 16 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -1936.431 
1000 observations: 626 selection 1 (FALSE) and 374 selection 2 (TRUE)
10 free parameters (df = 990)
Probit selection equation:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -0.3528     0.0424  -8.321 2.86e-16 ***
xs            0.8354     0.0756  11.050  < 2e-16 ***
Outcome equation 1:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.55448    0.06339  -8.748   <2e-16 ***
xs           0.81764    0.06048  13.519   <2e-16 ***
Outcome equation 2:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   0.6457     0.4994   1.293    0.196
xs            0.3520     0.3197   1.101    0.271
   Error terms:
       Estimate Std. Error t value Pr(>|t|)    
sigma1  0.59187    0.01853  31.935   <2e-16 ***
sigma2  1.97257    0.07228  27.289   <2e-16 ***
rho1    0.15568    0.15914   0.978    0.328    
rho2   -0.01541    0.23370  -0.066    0.947    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coef(summary(lm(yo1~xs, subset=ys==0)))
Estimate Std. Error   t value     Pr(>|t|)
(Intercept) -0.6109093 0.02462008 -24.81346 4.339319e-95
xs           0.7773989 0.04403325  17.65482 7.007157e-57

coef(summary(lm(yo2~xs, subset=ys==1)))
Estimate Std. Error t value
Pr(>|t|)
(Intercept) 0.6136357 0.1130488 5.428058 1.029707e-07
xs          0.3693103 0.1834578 2.013054 4.482896e-02


### Example                                                                ###
data( "Mroz87" )
str(Mroz87)
lfp: Dummy variable for labor-force participation
faminc: Family income, in 1975 dollars

Mroz87$kids <- ( Mroz87$kids5 + Mroz87$kids618 > 0 )
greeneTS <- selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
wage ~ exper + I( exper^2 ) + educ + city,
data = Mroz87, method = "2step" )
greeneTS
summary(greeneTS)

greeneML <- selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
wage ~ exper + I( exper^2 ) + educ + city, data = Mroz87,
maxMethod = "BHHH", iterlim = 500 )
summary(greeneML)

### Example
data( "RandHIE" )
str(RandHIE)
binexp: 1 if meddol > 0
idp: 1 if individual deductible plan
lnmeddol: log of meddol (medical expenses)

subsample <- RandHIE$year == 2 & !is.na( RandHIE$educdec )
selectEq <- binexp ~ logc + idp + lpi + fmde + physlm + disea +
hlthg + hlthf + hlthp + linc + lfam + educdec + xage + female +
child + fchild + black

outcomeEq <- lnmeddol ~ logc + idp + lpi + fmde + physlm + disea +
hlthg + hlthf + hlthp + linc + lfam + educdec + xage + female +
child + fchild + black
rhieTS <- selection( selectEq, outcomeEq, data = RandHIE[ subsample, ],
method = "2step" )
summary(rhieTS)

rhieML <- selection( selectEq, outcomeEq, data = RandHIE[ subsample, ] )
summary(rhieML)

### Robustness 

greeneStart <- selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
wage ~ exper + I( exper^2 ) + educ + city,
data = Mroz87, start = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.9))
cat( greeneStart$message )

 coef( summary( greeneStart ) )[ "rho", ]

set.seed(0)
greeneSANN <- selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
wage ~ exper + I( exper^2 ) + educ + city,
data = Mroz87, start = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.9),
maxMethod="SANN", parscale = 0.001 )
greeneStartSANN <- selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
wage ~ exper + I( exper^2 ) + educ + city,
data = Mroz87, start = coef( greeneSANN ) )
cat( greeneStartSANN$message )

logLik( greeneML )

logLik( greeneStartSANN )

set.seed(0)
x <- runif(1000)
y <- x + rnorm(1000)
ys <- y > 0
tobitML <- selection(ys~x, y~x)
cat( tobitML$message )

coef( summary( tobitML ) )[ "rho", ]
tobitTS <- selection(ys~x, y~x, method="2step")
coef( summary( tobitTS ) )[ "rho", ]

