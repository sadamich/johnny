https://cran.r-project.org/web/packages/psych/refman/psych.html#principal
library(psych)

#Four principal components of the Harman 24 variable problem
#compare to a four factor principal axes solution using factor.congruence
pc <- principal(Harman74.cor$cov,4,rotate="varimax")

https://cran.r-project.org/web/packages/psych/refman/psych.html#fa
https://www.geo.fu-berlin.de/en/v/soga-r/Advanced-statistics/Multivariate-approaches/Factor-Analysis/index.html

mr <- fa(Harman74.cor$cov,4,rotate="varimax")  #minres factor analysis
pa <- fa(Harman74.cor$cov,4,rotate="varimax",fm="pa")  # principal axis factor analysis
round(factor.congruence(list(pc,mr,pa)),2)
 RC1  RC3  RC2  RC4  MR1  MR3  MR2  MR4  PA1  PA3  PA2  PA4
RC1 1.00 0.53 0.43 0.46 1.00 0.61 0.46 0.54 1.00 0.61 0.46 0.54
RC3 0.53 1.00 0.43 0.47 0.54 0.99 0.44 0.54 0.54 0.99 0.44 0.54
RC2 0.43 0.43 1.00 0.47 0.44 0.50 1.00 0.55 0.44 0.50 1.00 0.55
RC4 0.46 0.47 0.47 1.00 0.47 0.53 0.49 0.99 0.47 0.53 0.49 0.99
MR1 1.00 0.54 0.44 0.47 1.00 0.61 0.46 0.55 1.00 0.61 0.46 0.55
MR3 0.61 0.99 0.50 0.53 0.61 1.00 0.50 0.61 0.61 1.00 0.50 0.61
MR2 0.46 0.44 1.00 0.49 0.46 0.50 1.00 0.57 0.46 0.50 1.00 0.57
MR4 0.54 0.54 0.55 0.99 0.55 0.61 0.57 1.00 0.55 0.61 0.57 1.00
PA1 1.00 0.54 0.44 0.47 1.00 0.61 0.46 0.55 1.00 0.61 0.46 0.55
PA3 0.61 0.99 0.50 0.53 0.61 1.00 0.50 0.61 0.61 1.00 0.50 0.61
PA2 0.46 0.44 1.00 0.49 0.46 0.50 1.00 0.57 0.46 0.50 1.00 0.57
PA4 0.54 0.54 0.55 0.99 0.55 0.61 0.57 1.00 0.55 0.61 0.57 1.00


pc2 <- principal(Harman.5,2,rotate="varimax")
pc2
Principal Components Analysis
Call: principal(r = Harman.5, nfactors = 2, rotate = "varimax")
Standardized loadings (pattern matrix) based upon correlation matrix
              RC1   RC2   h2    u2 com
population   0.02  0.99 0.99 0.012 1.0
schooling    0.94 -0.01 0.89 0.115 1.0
employment   0.14  0.98 0.98 0.021 1.0
professional 0.83  0.45 0.88 0.120 1.5
housevalue   0.97 -0.01 0.94 0.062 1.0

                       RC1  RC2
SS loadings           2.52 2.15
Proportion Var        0.50 0.43
Cumulative Var        0.50 0.93
Proportion Explained  0.54 0.46
Cumulative Proportion 0.54 1.00

Mean item complexity =  1.1
Test of the hypothesis that 2 components are sufficient.

The root mean square of the residuals (RMSR) is  0.02 
 with the empirical chi square  0.15  with prob <  0.7 

Fit based upon off diagonal values = 1> 

round(cor(Harman.5,pc2$scores),2)  #compare these correlations to the loadings 
       RC1   RC2
population   0.02  0.99
schooling    0.94 -0.01
employment   0.14  0.98
professional 0.83  0.45
housevalue   0.97 -0.01


#now do it for unstandardized scores, and transform obliquely
pc2o <- principal(Harman.5,2,rotate="promax",covar=TRUE)
pc2o
Principal Components Analysis
Call: principal(r = Harman.5, nfactors = 2, rotate = "promax", covar = TRUE)
Standardized loadings (pattern matrix) based upon correlation matrix
               RC1   RC2   h2    u2 com
population   -0.11  1.01 0.99 0.012 1.0
schooling     0.96 -0.13 0.89 0.115 1.0
employment    0.02  0.98 0.98 0.021 1.0
professional  0.79  0.35 0.88 0.120 1.4
housevalue    0.99 -0.13 0.94 0.062 1.0

                       RC1  RC2
SS loadings           2.53 2.14
Proportion Var        0.51 0.43
Cumulative Var        0.51 0.93
Proportion Explained  0.54 0.46
Cumulative Proportion 0.54 1.00

 With component correlations of 
     RC1  RC2
RC1 1.00 0.24
RC2 0.24 1.00

Mean item complexity =  1.1
Test of the hypothesis that 2 components are sufficient.

The root mean square of the residuals (RMSR) is  0.02 
 with the empirical chi square  0.15  with prob <  0.7 

Fit based upon off diagonal values = 1> 
round(cov(Harman.5,pc2o$scores),2) 
 RC1         RC2
population     -36540.11  7460683.30
schooling        3864.00     -266.10
employment     300199.01  2619927.43
professional   221355.65    89775.59
housevalue   15934475.60 -1192838.73

pc2o$Structure    #this matches the covariances with the scores

                   RC1       RC2
population   0.1380470 0.9884735
schooling    0.9325424 0.1044421
employment   0.2564425 0.9894333
professional 0.8735095 0.5431315
housevalue   0.9601422 0.1104942

biplot(pc2,main="Biplot of the Harman.5 socio-economic variables",labels=paste0(1:12))

#For comparison with SPSS  (contributed by Gottfried Helms)
pc2v <- principal(iris[1:4],2,rotate="varimax",normalize=FALSE,eps=1e-14)
print(pc2v,digits=7)
Principal Components Analysis
Call: principal(r = iris[1:4], nfactors = 2, rotate = "varimax", normalize = FALSE, 
    eps = 1e-14)
Standardized loadings (pattern matrix) based upon correlation matrix
                    RC1        RC2        h2          u2      com
Sepal.Length  0.9593182  0.0480332 0.9225986 0.077401362 1.005014
Sepal.Width  -0.1442733  0.9849389 0.9909193 0.009080678 1.042893
Petal.Length  0.9441083 -0.3039562 0.9837300 0.016270047 1.205101
Petal.Width   0.9323563 -0.2568893 0.9352804 0.064719625 1.150960

                            RC1       RC2
SS loadings           2.7017351 1.1307932
Proportion Var        0.6754338 0.2826983
Cumulative Var        0.6754338 0.9581321
Proportion Explained  0.7049485 0.2950515
Cumulative Proportion 0.7049485 1.0000000

Mean item complexity =  1.1
Test of the hypothesis that 2 components are sufficient.

The root mean square of the residuals (RMSR) is  0.0218326 
 with the empirical chi square  0.8579933  with prob <  NA 

Fit based upon off diagonal values = 0.9989358> 

pc2V <- principal(iris[1:4],2,rotate="Varimax",eps=1e-7)
Loading required namespace: GPArotation
p <- print(pc2V,digits=7)
Principal Components Analysis
Call: principal(r = iris[1:4], nfactors = 2, rotate = "Varimax", eps = 1e-07)
Standardized loadings (pattern matrix) based upon correlation matrix
                    RC1        RC2        h2          u2      com
Sepal.Length  0.9593182  0.0480331 0.9225986 0.077401362 1.005014
Sepal.Width  -0.1442732  0.9849389 0.9909193 0.009080678 1.042893
Petal.Length  0.9441083 -0.3039563 0.9837300 0.016270047 1.205101
Petal.Width   0.9323563 -0.2568893 0.9352804 0.064719625 1.150960

                            RC1       RC2
SS loadings           2.7017350 1.1307933
Proportion Var        0.6754338 0.2826983
Cumulative Var        0.6754338 0.9581321
Proportion Explained  0.7049485 0.2950515
Cumulative Proportion 0.7049485 1.0000000

Mean item complexity =  1.1
Test of the hypothesis that 2 components are sufficient.

The root mean square of the residuals (RMSR) is  0.0218326 
 with the empirical chi square  0.8579933  with prob <  NA 

Fit based upon off diagonal values = 0.9989358> 

round(p$Vaccounted,2)   # the amount of variance accounted for is returned as an object of print
                       RC1  RC2
SS loadings           2.70 1.13
Proportion Var        0.68 0.28
Cumulative Var        0.68 0.96
Proportion Explained  0.70 0.30
Cumulative Proportion 0.70 1.00