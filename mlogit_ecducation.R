### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 6 4 Bank wages ###
xm604<- read.csv("xm604.csv", header = TRUE)
attach(xm604)
str(xm604)
detach(xm604)

https://cran.r-project.org/web/packages/AER/refman/AER.html#BankWages
library(AER)
library("mlogit")
fm_mlogit <- mlogit(JOBCAT ~ 1 | EDUC + MINORITY, data = xm604,
  subset = GENDER == "1", shape = "wide", reflevel = "1")
summary(fm_mlogit)
### Panel 1 (p.472) Call:
mlogit(formula = JOBCAT ~ 1 | EDUC + MINORITY, data = xm604, 
    subset = GENDER == "1", reflevel = "1", shape = "wide", method = "nr")
Frequencies of alternatives:choice
      1       2       3 
0.60853 0.10465 0.28682 

nr method
8 iterations, 0h:0m:0s 
g'(-H)^-1g = 9.15E-06 
successive function values within tolerance limits 
Coefficients :
                Estimate Std. Error z-value  Pr(>|z|)    
(Intercept):2   4.760722   1.172774  4.0594 4.921e-05 ***
(Intercept):3 -26.014104   4.314443 -6.0295 1.644e-09 ***
EDUC:2         -0.553399   0.099041 -5.5876 2.303e-08 ***
EDUC:3          1.633370   0.276848  5.8999 3.638e-09 ***
MINORITY:2      0.426952   0.502708  0.8493  0.395712    
MINORITY:3     -2.109089   0.794193 -2.6556  0.007916 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -118.74
McFadden R^2:  0.48676 
Likelihood ratio test : chisq = 225.22 (p.value = < 2.22e-16)


fm_mlogit2 <- mlogit(JOBCAT ~ 1 | EDUC + MINORITY, data = xm604,
   shape = "wide", reflevel = "1")
summary(fm_mlogit2)
Call:
mlogit(formula = JOBCAT ~ 1 | EDUC + MINORITY, data = xm604, 
    reflevel = "1", shape = "wide", method = "nr")

Frequencies of alternatives:choice
       1        2        3 
0.765823 0.056962 0.177215 

nr method
8 iterations, 0h:0m:0s 
g'(-H)^-1g = 0.000175 
successive function values within tolerance limits 
Coefficients :
                Estimate Std. Error z-value  Pr(>|z|)    
(Intercept):2   2.232195   0.962219  2.3198  0.020350 *  
(Intercept):3 -28.036563   4.031870 -6.9537 3.557e-12 ***
EDUC:2         -0.455022   0.089611 -5.0778 3.819e-07 ***
EDUC:3          1.745239   0.257476  6.7783 1.216e-11 ***
MINORITY:2      1.174653   0.434734  2.7020  0.006892 ** 
MINORITY:3     -2.118933   0.759364 -2.7904  0.005264 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -165.79
McFadden R^2:  0.4812 
Likelihood ratio test : chisq = 307.55 (p.value = < 2.22e-16)

xm604_s<- subset(xm604, GENDER==1)
attach(xm604_s)
str(xm604_s)
data_s<- data.frame(OBS, JOBCAT, EDUC, MINORITY)

head(data_s, 10)
library(mlogit)
EDUC_1<- EDUC*DUMJCAT1
EDUC_2<- EDUC*DUMJCAT2
EDUC_3<- EDUC*DUMJCAT3
MINORITY_1<- MINORITY*DUMJCAT1
MINORITY_2<- MINORITY*DUMJCAT2
MINORITY_3<- MINORITY*DUMJCAT3
data_ed<- data.frame(OBS,JOBCAT,EDUC_1,EDUC_2,EDUC_3,
            MINORITY_1,MINORITY_2,MINORITY_3)
data_ed$OBS<- 1:nrow(data_ed)
head(data_ed, 10)
ED <- dfidx(data_ed,shape= "wide", varying = 3:8, sep ="_",
            choice = "JOBCAT"
)
m<- mlogit(JOBCAT~ 1|EDUC + MINORITY , ED)
summary(m)
noalt<- rep(3, 258)
jobcat<- dfidx(data_s, subset = noalt == 3, alt.levels = c("1", "2","3"))

library("Formula")
f<- Formula(JOBCAT ~ EDUC | MINORITY)
mf<- model.frame(jobcat,f)
md<- mlogit.data(data_s, JOBCAT, EDUC, MINORITY, shape = "wide", sep= FALSE)
????
head(ED, 10)


m<- mlogit(JOBCAT~1|EDUC|MINORITY,mf)
summary(m)
### Indexing                                                               ###
idx<- as.numeric(rownames(xm604))
JOBCAT<- JOBCAT[idx]
OBS<- OBS[idx]
xm604_i<- data.frame(xm604,JOBCAT_i)
EDUC<- EDUC[idx]
GENDER<- GENDER[idx]
MINORITY<- MINORITY[idx]


### Wide
xm604$JOBCAT <- 1:nrow(xm604)
head(xm604,3)
job<- dfidx(xm604, shape = "wide", varying = 11:13, sep = "_",
            idx = list(c("OBS", "JOBCAT")))

MC <- dfidx(xm604,alt.levels = c("DUMJCAT1","DUMJCAT2","DUMJCAT3"),idx="JOBCAT")
ml.MC1 <- mlogit( JOBCAT~ EDUC+MINORITY, MC)
summary(ml.MC1)
library(mlogit)
summary(mlogit(JOBCAT_i ~ EDUC_i+GENDER_i+MINORITY_i, data = xm604_i))???
sxm604<- subset(xm604, GENDER==1)
str(sxm604)
attach(sxm604)

data<- dfidx(sxm604, alt.levels = c(1,2,3))
library("Formula")
JOBCAT<- as.factor(JOBCAT)
f<- formula(JOBCAT ~ EDUC |MINORITY)



