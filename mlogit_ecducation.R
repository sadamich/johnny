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
newdata<- data.frame(OBS,DUMJCAT1,DUMJCAT2,DUMJCAT3,EDUC,MINORITY,JOBCAT)
library(mlogit)
xm604_i<- dfidx(newdata)
head(xm604_i,5)

JOBCAT<- factor(JOBCAT, labels=c("1","2","3"))
MC<- dfidx(JOBCAT)
newdata2<- data.frame(OBS,DUMJCAT2,DUMJCAT3,EDUC,MINORITY,JOBCAT)
xm604_i2<- dfidx(newdata2)
head(xm604_i2,5)
eq2<- mlogit(DUMJCAT2~EDUC+MINORITY)


jcat<- cbind(DUMJCAT1,DUMJCAT2,DUMJCAT3)
newdatax<- data.frame(OBS,JOBCAT,DUMJCAT1,DUMJCAT2,DUMJCAT3,EDUC,MINORITY)
xm604_ix<- dfidx(newdatax)
MC<- xm604_ix
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
