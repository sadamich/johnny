https://cran.r-project.org/web/packages/survival/vignettes/adjcurve.pdf


###  marginal analysis

### understand and model the effect of each confounder:the conditional approach

###  division of the covariates x into effects of interest vs.confounders


Marginal: balance data on x −→ form survival curves for each c
Conditional:predicted curves for {x,c}subset→average the predictions for each c


### 2 Free Light Chain

### Figure 1: Survival of 7874 residents of Olmsted County, broken        ###
###  into three cohorts based on FLC value.                               ###

fdata <- flchain[flchain$futime >=7,]
fdata$age2 <- cut(fdata$age, c(0,54, 59,64, 69,74,79, 89, 110),
labels = c(paste(c(50,55,60,65,70,75,80),
c(54,59,64,69,74,79,89), sep='-'), "90+"))
fdata$group <- factor(1+ 1*(fdata$flc.grp >7) + 1*(fdata$flc.grp >9),
levels=1:3,
labels=c("FLC < 3.38", "3.38- 4.71", "FLC > 4.71"))
sfit1 <- survfit(Surv(futime, death) ~ group, fdata)
plot(sfit1, mark.time=F, col=c(1,2,4), lty=1, lwd=2,
xscale=365.25, xlab="Years from Sample",
ylab="Survival")
text(c(11.1, 10.5, 7.5)*365.25, c(.88, .57, .4),
c("FLC < 3.38", "3.38- 4.71", "FLC > 4.71"), col=c(1,2,4))

### 3 Reference populations

### 4 Marginal approach
### 4.1 Selection


### Figure 2: Survival curves from a case-control sample are shown as 
### solid lines, dashed lines are curves for the unweighted data set 
### (as found in figure 1)

temp <-with(fdata, table(group, age2, sex))
dd <-dim(temp)
# Selectsubjects
set.seed(1978)
select <-array(vector('list', length=prod(dd)), dim=dd)
for (j in 1:dd[2]) {
   for (k in 1:dd[3]) {
      n <- temp[3,j,k] # how many toselect
            for (i in 1:2) {
            indx <- which(as.numeric(fdata$group)==i &
            as.numeric(fdata$age2) ==j &
            as.numeric(fdata$sex) ==k)
            select[i,j,k] <-list(sample(indx, n, replace=(n> temp[i,j,k])))
}
indx <- which(as.numeric(fdata$group)==3 &
as.numeric(fdata$age2) ==j &
as.numeric(fdata$sex)==k)
select[3,j,k] <-list(indx) #keep allthe group 3 = high
}
}
data2 <-fdata[unlist(select),]
sfit2 <-survfit(Surv(futime, death)~ group, data2)
plot(sfit2,col=c(1,2,4), lty=1, lwd=2,
xscale=365.25,xlab="Years from Sample",
ylab="Survival")
lines(sfit1,col=c(1,2,4), lty=2, lwd=1,
xscale=365.25)
legend(730,.4, levels(fdata$group), lty=1, col=c(1,2,4),bty='n', lwd=2)
survdiff(Surv(futime, death) ~ group, data=data2)


Call:
survdiff(formula = Surv(futime, death) ~ group, data = data2)
                   N Observed Expected (O-E)^2/E (O-E)^2/V
group=FLC < 3.38 758      341      456     29.17     47.37
group=3.38- 4.71 758      377      415      3.54      5.43
group=FLC > 4.71 758      477      323     73.08    100.71
Chisq= 106  on 2 degrees of freedom, p= <2e-16 

### 4.2 Reweighting