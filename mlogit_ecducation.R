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



