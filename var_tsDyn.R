### Package tsDyn  ###
### Source: https://cran.r-project.org/web/packages/tsDyn/refman/tsDyn.html ###
library(tsDyn)
### zeroyld contains two variables, while zeryldMeta contains also Year     ###
### and Month columns.                                                      ###
### zeroyld is a data frame with 482 observations and 2 variables:          ###
### short.run	numeric	Short term, 12 month                          ###
### long.run	numeric	Long term, 120 month                          ###
BBCTest(lynx, m=3, test="Wald", grid="minPerc")

data(zeroyld)
data<-zeroyld
#Fit a VECM with Engle-Granger 2OLS estimator:
vecm.eg<-VECM(zeroyld, lag=2)
#Fit a VECM with Johansen MLE estimator:
vecm.jo<-VECM(zeroyld, lag=2, estim="ML")

#compare results with package vars:
if(require(vars)) {
 data(finland)
 #check long coint values
   all.equal(VECM(finland, lag=2, estim="ML", r=2)$model.specific$beta, 
             cajorls(ca.jo(finland, K=3, spec="transitory"), r=2)  $beta, check.attributes=FALSE)
# check OLS parameters
  all.equal(t(coefficients(VECM(finland, lag=2, estim="ML", r=2))), 
    coefficients(cajorls(ca.jo(finland, K=3, spec="transitory"), r=2)$rlm), check.attributes=FALSE)

}
##export to Latex
toLatex(vecm.eg)
toLatex(summary(vecm.eg))
options("show.signif.stars"=FALSE)
toLatex(summary(vecm.eg), parenthese="Pvalue")
options("show.signif.stars"=TRUE)
data(barry)
vecm <- VECM(barry,  lag=1, estim="ML")
vecm_r2 <- VECM(barry,  lag=1, estim="ML", r=2)

## extract coefficients:
coefA(vecm)
coefB(vecm)
coefPI(vecm)
coefB(vecm_r2)
coefPI(vecm_r2)

## Beta-Restricted VECM:
beta_vecm2 <- coefB(vecm_r2) 
beta_vecm2[3,2] <- 0.02
vecm_r2_rest <- VECM(barry,  lag=1, estim="ML", r=2, beta=beta_vecm2)
round(coefB(vecm_r2_rest),5)

## Package vars/urca
if(require(urca)){
 vecm_ur <- ca.jo(barry, K=2)
 coefB(vecm_ur)
 coefB(vecm_ur,r=2)
 coefB(cajorls(vecm_ur, r=2))
 all.equal(coefB(vecm), coefB(vecm_ur), check.attributes=FALSE)
 all.equal(coefB(vecm_r2), coefB(vecm_ur, r=2), check.attributes=FALSE)
}

data(zeroyld)

#Fit a VAR
VAR <- lineVar(zeroyld, lag=1)
VAR
summary(VAR)

#compare results with package vars:
if(require(vars)) {
a<-VAR(zeroyld, p=1)
coef_vars <- t(sapply(coef(a), function(x) x[c(3,1,2),1]))
all.equal(coef(VAR),coef_vars, check.attributes=FALSE)
}

###VECM
VECM.EG <- lineVar(zeroyld, lag=2, model="VECM")
VECM.EG
summary(VECM.EG)

VECM.ML <- lineVar(zeroyld, lag=2, model="VECM", estim="ML")
VECM.ML
summary(VECM.ML)


###Check Johansen MLE
myVECM <- lineVar(zeroyld, lag=1, include="const", model="VECM", estim="ML")
summary(myVECM, digits=7) 
#comparing with vars package
if(require(vars)){
a<-ca.jo(zeroyld, spec="trans")
summary(a)
#same answer also!
}

##export to Latex
toLatex(VECM.EG)
toLatex(summary(VECM.EG))
options("show.signif.stars"=FALSE)
toLatex(summary(VECM.EG), parenthese="Pvalue")
options("show.signif.stars"=TRUE)




