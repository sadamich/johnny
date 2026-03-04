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
Tobit 2 model (sample selection model)
2-step Heckman / heckit estimation
753 observations (325 censored and 428 observed)
14 free parameters (df = 740)
Probit selection equation:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -4.157e+00  1.402e+00  -2.965 0.003127 ** 
age          1.854e-01  6.597e-02   2.810 0.005078 ** 
I(age^2)    -2.426e-03  7.735e-04  -3.136 0.001780 ** 
faminc       4.580e-06  4.206e-06   1.089 0.276544    
kidsTRUE    -4.490e-01  1.309e-01  -3.430 0.000638 ***
educ         9.818e-02  2.298e-02   4.272 2.19e-05 ***
Outcome equation:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.9712003  2.0593505  -0.472    0.637    
exper        0.0210610  0.0624646   0.337    0.736    
I(exper^2)   0.0001371  0.0018782   0.073    0.942    
educ         0.4170174  0.1002497   4.160 3.56e-05 ***
city         0.4438379  0.3158984   1.405    0.160    
Multiple R-Squared:0.1264,      Adjusted R-Squared:0.116
   Error terms:
              Estimate Std. Error t value Pr(>|t|)
invMillsRatio   -1.098      1.266  -0.867    0.386
sigma            3.200         NA      NA       NA
rho             -0.343         NA      NA       NA

greeneML <- selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
wage ~ exper + I( exper^2 ) + educ + city, data = Mroz87,
maxMethod = "BHHH", iterlim = 500 )
summary(greeneML)
Tobit 2 model (sample selection model)
Maximum Likelihood estimation
BHHH maximisation, 62 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -1581.259 
753 observations (325 censored and 428 observed)
13 free parameters (df = 740)
Probit selection equation:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -4.120e+00  1.410e+00  -2.921  0.00359 ** 
age          1.840e-01  6.584e-02   2.795  0.00532 ** 
I(age^2)    -2.409e-03  7.735e-04  -3.115  0.00191 ** 
faminc       5.676e-06  3.890e-06   1.459  0.14493    
kidsTRUE    -4.507e-01  1.367e-01  -3.298  0.00102 ** 
educ         9.533e-02  2.400e-02   3.973  7.8e-05 ***
Outcome equation:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.9537242  1.6745690  -1.167    0.244    
exper        0.0284295  0.0753989   0.377    0.706    
I(exper^2)  -0.0001151  0.0023339  -0.049    0.961    
educ         0.4562471  0.0959626   4.754 2.39e-06 ***
city         0.4451424  0.4255420   1.046    0.296    
   Error terms:
      Estimate Std. Error t value Pr(>|t|)    
sigma  3.10350    0.08368  37.088   <2e-16 ***
rho   -0.13328    0.22296  -0.598     0.55    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### Example
data( "RandHIE" )
str(RandHIE)
### Selection equation ###
meddol: Medical expenses: all covered inpatient and outpatient services, 
       including drugs, supplies, and inpatient costs of newborns excluding 
       dental care and outpatient psychotherapy
binexp: 1 if meddol > 0
idp: 1 if individual deductible plan
lpi: log of pioff (participation incentive payment)
fmde: 0 if idp=1, ln(max(1,mdeoff/(0.01*coins))) otherwise
physlm: Physical limitations
disea: Number of chronic diseases
hlthg: 1 if self-rated health is good – baseline is excellent self-rated health.
hlthf: 1 if self-rated health is fair – baseline is excellent self-rated health.
hlthp: 1 if self-rated health is poor – baseline is excellent self-rated health
linc: log of income (family income)
lfam: log of num (family size)
educdec: Education of household head in years
xage: Age in years
female: 1 if person is female
child: 1 if age is less than 18 years.
fchild:female * child
black: 1 if race of household head is black
### Outcome equation                                                       ###
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

