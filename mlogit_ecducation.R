### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 6 4 Bank wages ###
xm604<- read.csv("xm604.csv", header = TRUE)
attach(xm604)
str(xm604)
detach(xm604)
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
jobcat<- factor(JOBCAT, levels = c("1","2","3"))
str(jobcat)
Call:
mlogit(formula = JOBCAT ~ 1 | EDUC | MINORITY, data = ED, method = "nr")

Frequencies of alternatives:choice
      1       2       3 
0.33333 0.33333 0.33333 

nr method
1 iterations, 0h:0m:0s 
g'(-H)^-1g = 1E+10 
successive function values within tolerance limits 

Coefficients :
              Estimate Std. Error z-value Pr(>|z|)
(Intercept):2 0.000000   0.734483       0        1
(Intercept):3 0.000000   0.739815       0        1
EDUC:2        0.000000   0.054699       0        1
EDUC:3        0.000000   0.017369       0        1
MINORITY:1    0.000000   0.797108       0        1
MINORITY:2    0.000000   0.783476       0        1
MINORITY:3    0.000000   1.064001       0        1

Log-Likelihood: 7.7716e-16
McFadden R^2:  -Inf 
Likelihood ratio test : chisq = 1700.7 (p.value = < 2.22e-16)
> 


method = c("bfgs", "nr", "bhhh")
nojob<- rep(3, 258)
data_s<- data.frame(OBS,JOBCAT,EDUC,MINORITY,nojob)
ED<- dfidx(data_s)
attach(ED)
str(JOBCAT)
library("Formula")
f<- Formula(JOBCAT ~ EDUC | MINORITY)
mf<- model.frame(ED,f)
md<- mlogit.data(data_s, JOBCAT, EDUC, MINORITY, shape = "wide", sep= FALSE)
????
head(ED, 10)

xm604_s$OBS<- 1:nrow(xm604_s)
head(xm604_s,5)
newdata<- data.frame(OBS,DUMJCAT2,DUMJCAT3,DUMJCAT1,EDUC,MINORITY, JOBCAT)
head(newdata, 5)
JOBCAT<- factor(JOBCAT, labels=c("1","2","3"))
MC<- dfidx(newdata,drop.index=FALSE)
head(MC,5)
eqx<- mlogit(DUMJCAT2~EDUC+MINORITY,MC)
summary(eqx)
### Indexing                                                               ###
idx<- as.numeric(rownames(xm604))
JOBCAT<- JOBCAT[idx]
OBS<- OBS[idx]
xm604_i<- data.frame(xm604,JOBCAT_i)
EDUC<- EDUC[idx]
GENDER<- GENDER[idx]
MINORITY<- MINORITY[idx]


### Subset GENDER == 1                                                     ###
JOBCAT<- JOBCAT[GENDER==1]
EDUC<- EDUC[GENDER==1]
MINORITY<- MINORITY[GENDER==1]
DUMJCAT1<- DUMJCAT1[GENDER==1]
DUMJCAT2<- DUMJCAT2[GENDER==1]
DUMJCAT3<- DUMJCAT3[GENDER==1]
JOBCAT_f<- factor(JOBCAT,labels=c("1","2","3"))
OBS<- OBS[GENDER==1]
job_s<- data.frame(DUMJCAT1,DUMJCAT2,DUMJCAT3,EDUC,MINORITY,JOBCAT_f,OBS)
head(job_s,5)
  DUMJCAT1 DUMJCAT2 DUMJCAT3 EDUC MINORITY JOBCAT_f OBS
1        0        0        1   15        0        3   1
2        1        0        0   16        0        1   2
3        1        0        0   15        1        1   7
4        1        0        0    8        0        1  12
5        1        0        0   15        0        3  13

job_m<- mlogit.data(job_s, choice = c("DUMJCAT1","DUMJCAT2","DUMJCAT3"),
shape= "long")
            
JOBCAT_f<- JOBCAT[idx]
DUMJCAT2<- DUMJCAT2[idx]
job_m<- data.frame(DUMJCAT1,DUMJCAT2,DUMJCAT3,EDUC,MINORITY, JOBCAT_f)
MC <- dfidx(job_s, subset = JOBCAT_f == 3, idx=c("JOBCAT_f","DUMJCAT2"))
job_mlogit<- mlogit(DUMJCAT2~ EDUC+MINORITY,xm604_i, JOBCAT_f)
summary(eq)
mnl<- function(theta){
beta2_1<- theta[1]
beta2_2<- theta[2]
beta2_3<- theta[3]
beta3_1<- theta[4]
beta3_2<- theta[5]
beta3_3<- theta[6]
sum(JOBCAT*(beta2_1+beta2_2*EDUC+beta2_3*MINORITY)
   +JOBCAT*(beta3_1+beta3_2*EDUC+beta2_3*MINORITY)
   - log(1+ exp(beta2_1+beta2_2*EDUC+beta2_3*MINORITY)
      +exp(beta3_1+beta3_2*EDUC+beta2_3*MINORITY)))
}
library(maxLik)
m_mnl<- maxLik(mnl, start= c(0,1,1,0,1,1))
summary(m_mnl)
co <- maxControl(printLevel=2, qac="marquardt", marquardt_lambda0=1)
eq <- maxNR(mnl, start=c(1,1,1,1,1,1), control=co)
summary(eq)


data_m<- mlogit.data(xm604, choice=NULL,alt.var=JOBCAT,

str(data_m)
head(data_m,5)
head(data_m, 50:55)
library(mlogit)
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

eq1<- mlogit(f,data)
summary(eq1)


MC <- dfidx(ModeCanada, subset = noalt == 4,
            alt.levels = c("train", "air", "bus", "car"))

f <- Formula(choice ~ cost | income + urban | ivt)
f2 <- Formula(choice ~ cost + ivt | income + urban)
f2 <- Formula(choice ~ cost + ivt | income + urban | 0)
f3 <- Formula(choice ~ 0 | income | 0)
eq_ex<- mlogit(f, MC)
summary(eq_ex)
