### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 6 4 Bank wages ###
xm604<- read.csv("xm604.csv", header = TRUE)
attach(xm604)
str(xm604)
detach(xm604)
idx<- as.numeric(rownames(xm604))
JOBCAT_i<- JOBCAT[idx]
JOBCAT_i
xm604_i<- data.frame(xm604,JOBCAT_i)
EDUC_i<- EDUC[idx]
GENDER_i<- GENDER[idx]
MINORITY_i<- MINORITY[idx]

library(mlogit)
MC <- dfidx(xm604, subset = JOBCAT == 3,idx = "JOBCAT", alt.levels = c("1", "2", "3"))
ml.MC1 <- mlogit( JOBCAT~ EDUC+GENDER, MC)
summary(ml.MC1)
library(mlogit)
summary(mlogit(JOBCAT_i ~ EDUC_i+GENDER_i+MINORITY_i, data = xm604_i))???
sxm604<- subset(xm604, GENDER==1)
str(sxm604)
attach(sxm604)

data<- dfidx(sxm604, subset = JOBCAT == 3, alt.levels = c(1,2,3))
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
