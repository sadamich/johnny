https://cran.r-project.org/web/packages/car/refman/car.html#Anova
library(car)
## Two-Way Anova
mod <- lm(conformity ~ fcategory*partner.status, data=Moore,
  contrasts=list(fcategory=contr.sum, partner.status=contr.sum))
attach(Moore)
str(Moore)
'data.frame':   45 obs. of  4 variables:
 $ partner.status: Factor w/ 2 levels "high","low": 2 2 2 2 2 2 2 2 2 2 ...
 $ conformity    : int  8 4 8 7 10 6 12 4 13 12 ...
 $ fcategory     : Factor w/ 3 levels "high","low","medium": 2 1 1 2 2 2 3 3 2 2 ...
 $ fscore        : int  37 57 65 20 36 18 51 44 31 36 ..
summary(mod)
Call:
lm(formula = conformity ~ fcategory * partner.status, data = Moore, 
    contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
Residuals:
    Min      1Q  Median      3Q     Max 
-8.6250 -2.9000 -0.2727  2.7273 11.3750 

Coefficients:
                           Estimate Std. Error t value Pr(>|t|)    
(Intercept)                 12.0508     0.7275  16.564  < 2e-16 ***
fcategory1                   0.1903     0.9987   0.191  0.84990    
fcategory2                   1.0992     1.0264   1.071  0.29080    
partner.status1              2.4591     0.7275   3.380  0.00166 ** 
fcategory1:partner.status1  -2.8431     0.9987  -2.847  0.00701 ** 
fcategory2:partner.status1   1.7909     1.0264   1.745  0.08890 .  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 4.579 on 39 degrees of freedom
Multiple R-squared:  0.3237,    Adjusted R-squared:  0.237 
F-statistic: 3.734 on 5 and 39 DF,  p-value: 0.007397

Anova(mod)
Anova Table (Type II tests)

Response: conformity
                         Sum Sq Df F value   Pr(>F)   
fcategory                 11.61  2  0.2770 0.759564   
partner.status           212.21  1 10.1207 0.002874 **
fcategory:partner.status 175.49  2  4.1846 0.022572 * 
Residuals                817.76 39                    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 

Anova(mod, type=3)  # note use of contr.sum in call to lm()
Anova Table (Type III tests)
Response: conformity
                         Sum Sq Df  F value    Pr(>F)    
(Intercept)              5752.8  1 274.3592 < 2.2e-16 ***
fcategory                  36.0  2   0.8589  0.431492    
partner.status            239.6  1  11.4250  0.001657 ** 
fcategory:partner.status  175.5  2   4.1846  0.022572 *  
Residuals                 817.8 39                       
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## use of vcov.; the following are equivalent:
Anova(mod, white.adjust = TRUE)
Coefficient covariances computed by hccm()
Analysis of Deviance Table (Type II tests)
Response: conformity
                         Df       F    Pr(>F)    
fcategory                 2  0.3766 0.6886545    
partner.status            1 14.0454 0.0005775 ***
fcategory:partner.status  2  2.8294 0.0712175 .  
Residuals                39                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 

Anova(mod, vcov. = hccm) # vcov. is a function, type = "hc3" is the default
Anova(mod, vcov. = hccm(mod, type = "hc3")) # vcov. is a matrix
Anova(mod, vcov. = function(m) hccm(m, type = "hc3")) # passing type as an argument

## One-Way MANOVA
## See ?Pottery for a description of the data set used in this example.

summary(Anova(lm(cbind(Al, Fe, Mg, Ca, Na) ~ Site, data=Pottery)))
attach(Pottery)
str(Pottery)
'data.frame':   26 obs. of  6 variables:
 $ Site: Factor w/ 4 levels "AshleyRails",..: 4 4 4 4 4 4 4 4 4 4 ...
 $ Al  : num  14.4 13.8 14.6 11.5 13.8 10.9 10.1 11.6 11.1 13.4 ...
 $ Fe  : num  7 7.08 7.09 6.37 7.06 6.26 4.26 5.78 5.49 6.92 ...
 $ Mg  : num  4.3 3.43 3.88 5.64 5.34 3.47 4.26 5.91 4.52 7.23 ...
 $ Ca  : num  0.15 0.12 0.13 0.16 0.2 0.17 0.2 0.18 0.29 0.28 ...
 $ Na  : num  0.51 0.17 0.2 0.14 0.2 0.22 0.18 0.16 0.3 0.2 ...
## MANOVA for a randomized block design (example courtesy of Michael Friendly:
##  See ?Soils for description of the data set)


soils.mod <- lm(cbind(pH,N,Dens,P,Ca,Mg,K,Na,Conduc) ~ Block + Contour*Depth,
    data=Soils)
Manova(soils.mod)
Type II MANOVA Tests: Pillai test statistic
              Df test stat approx F num Df den Df    Pr(>F)    
Block          3    1.6758   3.7965     27     81 1.777e-06 ***
Contour        2    1.3386   5.8468     18     52 2.730e-07 ***
Depth          3    1.7951   4.4697     27     81 8.777e-08 ***
Contour:Depth  6    1.2351   0.8640     54    180    0.7311    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘

summary(Anova(soils.mod), univariate=TRUE, multivariate=FALSE,
    p.adjust.method=TRUE)

## a multivariate linear model for repeated-measures data
## See ?OBrienKaiser for a description of the data set used in this example.

phase <- factor(rep(c("pretest", "posttest", "followup"), c(5, 5, 5)),
    levels=c("pretest", "posttest", "followup"))
hour <- ordered(rep(1:5, 3))
idata <- data.frame(phase, hour)
idata
 phase hour
1   pretest    1
2   pretest    2
3   pretest    3
4   pretest    4
5   pretest    5
6  posttest    1
7  posttest    2
8  posttest    3
9  posttest    4
10 posttest    5
11 followup    1
12 followup    2
13 followup    3
14 followup    4
15 followup    5
mod.ok <- lm(cbind(pre.1, pre.2, pre.3, pre.4, pre.5,
                     post.1, post.2, post.3, post.4, post.5,
                     fup.1, fup.2, fup.3, fup.4, fup.5) ~  treatment*gender,
                data=OBrienKaiser)
(av.ok <- Anova(mod.ok, idata=idata, idesign=~phase*hour))
Type II Repeated Measures MANOVA Tests: Pillai test statistic
                            Df test stat approx F num Df den Df    Pr(>F)    
(Intercept)                  1   0.96954   318.34      1     10 6.532e-09 ***
treatment                    2   0.48092     4.63      2     10 0.0376868 *  
gender                       1   0.20356     2.56      1     10 0.1409735    
treatment:gender             2   0.36350     2.86      2     10 0.1044692    
phase                        1   0.85052    25.61      2      9 0.0001930 ***
treatment:phase              2   0.68518     2.61      4     20 0.0667354 .  
gender:phase                 1   0.04314     0.20      2      9 0.8199968    
treatment:gender:phase       2   0.31060     0.92      4     20 0.4721498    
hour                         1   0.93468    25.04      4      7 0.0003043 ***
treatment:hour               2   0.30144     0.35      8     16 0.9295212    
gender:hour                  1   0.29274     0.72      4      7 0.6023742    
treatment:gender:hour        2   0.57022     0.80      8     16 0.6131884    
phase:hour                   1   0.54958     0.46      8      3 0.8324517    
treatment:phase:hour         2   0.66367     0.25     16      8 0.9914415    
gender:phase:hour            1   0.69505     0.85      8      3 0.6202076    
treatment:gender:phase:hour  2   0.79277     0.33     16      8 0.9723693    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(av.ok, multivariate=FALSE)
Univariate Type II Repeated-Measures ANOVA Assuming Sphericity

                            Sum Sq num Df Error SS den Df  F value    Pr(>F)
(Intercept)                 7260.0      1  228.056     10 318.3435 6.532e-09
treatment                    211.3      2  228.056     10   4.6323  0.037687
gender                        58.3      1  228.056     10   2.5558  0.140974
treatment:gender             130.2      2  228.056     10   2.8555  0.104469
phase                        167.5      2   80.278     20  20.8651 1.274e-05
treatment:phase               78.7      4   80.278     20   4.8997  0.006426
gender:phase                   1.7      2   80.278     20   0.2078  0.814130
treatment:gender:phase        10.2      4   80.278     20   0.6366  0.642369
hour                         106.3      4   62.500     40  17.0067 3.191e-08
treatment:hour                 1.2      8   62.500     40   0.0929  0.999257
gender:hour                    2.6      4   62.500     40   0.4094  0.800772
treatment:gender:hour          7.8      8   62.500     40   0.6204  0.755484
phase:hour                    11.1      8   96.167     80   1.1525  0.338317
treatment:phase:hour           6.3     16   96.167     80   0.3256  0.992814
gender:phase:hour              6.6      8   96.167     80   0.6900  0.699124
treatment:gender:phase:hour   14.2     16   96.167     80   0.7359  0.749562
                               
(Intercept)                 ***
treatment                   *  
gender                         
treatment:gender               
phase                       ***
treatment:phase             ** 
gender:phase                   
treatment:gender:phase         
hour                        ***
treatment:hour                 
gender:hour                    
treatment:gender:hour          
phase:hour                     
treatment:phase:hour           
gender:phase:hour              
treatment:gender:phase:hour    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Mauchly Tests for Sphericity

                            Test statistic p-value
phase                              0.74927 0.27282
treatment:phase                    0.74927 0.27282
gender:phase                       0.74927 0.27282
treatment:gender:phase             0.74927 0.27282
hour                               0.06607 0.00760
treatment:hour                     0.06607 0.00760
gender:hour                        0.06607 0.00760
treatment:gender:hour              0.06607 0.00760
phase:hour                         0.00478 0.44939
treatment:phase:hour               0.00478 0.44939
gender:phase:hour                  0.00478 0.44939
treatment:gender:phase:hour        0.00478 0.44939


Greenhouse-Geisser and Huynh-Feldt Corrections
 for Departure from Sphericity

                             GG eps Pr(>F[GG])    
phase                       0.79953  7.323e-05 ***
treatment:phase             0.79953    0.01223 *  
gender:phase                0.79953    0.76616    
treatment:gender:phase      0.79953    0.61162    
hour                        0.46028  8.741e-05 ***
treatment:hour              0.46028    0.97879    
gender:hour                 0.46028    0.65346    
treatment:gender:hour       0.46028    0.64136    
phase:hour                  0.44950    0.34573    
treatment:phase:hour        0.44950    0.94019    
gender:phase:hour           0.44950    0.58903    
treatment:gender:phase:hour 0.44950    0.64634    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                               HF eps   Pr(>F[HF])
phase                       0.9278594 2.387543e-05
treatment:phase             0.9278594 8.089765e-03
gender:phase                0.9278594 7.984495e-01
treatment:gender:phase      0.9278594 6.319975e-01
hour                        0.5592802 2.014357e-05
treatment:hour              0.5592802 9.887716e-01
gender:hour                 0.5592802 6.911521e-01
treatment:gender:hour       0.5592802 6.692976e-01
phase:hour                  0.7330608 3.440460e-01
treatment:phase:hour        0.7330608 9.804731e-01
gender:phase:hour           0.7330608 6.552382e-01
treatment:gender:phase:hour 0.7330608 7.080122e-01
 

## A "doubly multivariate" design with two  distinct repeated-measures variables
## (example courtesy of Michael Friendly)
## See ?WeightLoss for a description of the dataset.

imatrix <- matrix(c(
	1,0,-1, 1, 0, 0,
	1,0, 0,-2, 0, 0,
	1,0, 1, 1, 0, 0,
	0,1, 0, 0,-1, 1,
	0,1, 0, 0, 0,-2,
	0,1, 0, 0, 1, 1), 6, 6, byrow=TRUE)
colnames(imatrix) <- c("WL", "SE", "WL.L", "WL.Q", "SE.L", "SE.Q")
rownames(imatrix) <- colnames(WeightLoss)[-1]
(imatrix <- list(measure=imatrix[,1:2], month=imatrix[,3:6]))
$measure
    WL SE
wl1  1  0
wl2  1  0
wl3  1  0
se1  0  1
se2  0  1
se3  0  1

$month
    WL.L WL.Q SE.L SE.Q
wl1   -1    1    0    0
wl2    0   -2    0    0
wl3    1    1    0    0
se1    0    0   -1    1
se2    0    0    0   -2
se3    0    0    1    1

contrasts(WeightLoss$group) <- matrix(c(-2,1,1, 0,-1,1), ncol=2)
(wl.mod<-lm(cbind(wl1, wl2, wl3, se1, se2, se3)~group, data=WeightLoss))
Call:
lm(formula = cbind(wl1, wl2, wl3, se1, se2, se3) ~ group, data = WeightLoss)
Coefficients:
             wl1       wl2       wl3       se1       se2       se3     
(Intercept)   5.34444   4.45000   2.17778  14.92778  13.79444  16.28333
group1        0.42222   0.55833   0.04722   0.08889  -0.26944   0.60000
group2        0.43333   1.09167  -0.02500   0.18333  -0.22500   0.71667

Anova(wl.mod, imatrix=imatrix, test="Roy")
Type II Repeated Measures MANOVA Tests: Roy test statistic
              Df test stat approx F num Df den Df    Pr(>F)    
measure        1    86.203  1293.04      2     30 < 2.2e-16 ***
group:measure  2     0.356     5.52      2     31  0.008906 ** 
month          1     9.407    65.85      4     28 7.807e-14 ***
group:month    2     1.772    12.84      4     29 3.909e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## mixed-effects models examples:
## Not run:  # loads nlme package
	library(nlme)
	example(lme)
	Anova(fm2)
## End(Not run)
## Not run:  # loads lme4 package
	library(lme4)
	example(glmer)
	Anova(gm1)
## End(Not run)
