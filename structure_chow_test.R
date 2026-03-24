### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### 3 4 4 Illustration Bank wages (p.175)                                  ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
panel01<- lm(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY)
summary(panel01)
LOGSAL_12<- LOGSAL[JOBCAT!=2]
EDUC_12<- EDUC[JOBCAT!=2]
LOGSALBEGIN_12<- LOGSALBEGIN[JOBCAT!=2]
GENDER_12<- GENDER[JOBCAT!=2]
MINORITY_12<- MINORITY[JOBCAT!=2]
panel02<- lm(LOGSAL_12 ~ EDUC_12 + LOGSALBEGIN_12 + GENDER_12 + MINORITY_12)
summary(panel02)
LOGSAL_3<- LOGSAL[JOBCAT!=3]
EDUC_3<- EDUC[JOBCAT!=3]
LOGSALBEGIN_3<- LOGSALBEGIN[JOBCAT!=3]
GENDER_3<- GENDER[JOBCAT!=3]
MINORITY_3<- MINORITY[JOBCAT!=3]
panel03<- lm(LOGSAL_3 ~ EDUC_3 + LOGSALBEGIN_3 + GENDER_3 + MINORITY_3)
summary(panel03)

anova(panel01,panel03)
anova(panel01,panel02)
Analysis of Variance Table
Response: LOGSAL
             Df Sum Sq Mean Sq   F value  Pr(>F)    
EDUC          1 36.251  36.251 1162.2975 < 2e-16 ***
LOGSALBEGIN   1 23.532  23.532  754.5172 < 2e-16 ***
GENDER        1  0.129   0.129    4.1317 0.04265 *  
MINORITY      1  0.135   0.135    4.3382 0.03781 *  
Residuals   469 14.627   0.031                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"LOGSAL_12"’ removed because 
response differs from model 1
anova(panel01,panel02,panel03)
Analysis of Variance Table
Response: LOGSAL
             Df Sum Sq Mean Sq   F value  Pr(>F)    
EDUC          1 36.251  36.251 1162.2975 < 2e-16 ***
LOGSALBEGIN   1 23.532  23.532  754.5172 < 2e-16 ***
GENDER        1  0.129   0.129    4.1317 0.04265 *  
MINORITY      1  0.135   0.135    4.3382 0.03781 *  
Residuals   469 14.627   0.031                      
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘c("LOGSAL_12", "LOGSAL_3")’                     

### Exsample 5 9 Bank wages (p.317)                                        ###
xm501<- read.csv("xm501.csv",header =TRUE)
str(xm501)
attach(xm501)
panel01<- lm(LOGSALARY ~ GENDER+MINORITY+EDUC)
LOGSALARY_2<- LOGSALARY[EDUC<=16]
GENDER_2<- GENDER[EDUC<=16]
MINORITY_2<- MINORITY[EDUC<=16]
EDUC_2<- EDUC[EDUC<=16]
panel02<- lm(LOGSALARY_2~GENDER_2+MINORITY_2+EDUC_2)
LOGSALARY_3<- LOGSALARY[EDUC>=17]
GENDER_3<- GENDER[EDUC>=17]
MINORITY_3<- MINORITY[EDUC>=17]
EDUC_3<- EDUC[EDUC>=17]
panel03<- lm(LOGSALARY_3~GENDER_3+MINORITY_3+EDUC_3)
anova(panel01,panel02,panel03)
res_2<- resid(panel02)
ssr_2<- sum(res_2^2)
[1] 23.40327
res_3<- resid(panel03)
ssr_3<- sum(res_3^2)
[1] 2.941173
### 5 20 (p.315)                                                           ###
F_break<- function(ssr, ssr2,ssr3,k,n2,n3){
result<- ((ssr - ssr2 -ssr3)/k) / ((ssr2+ssr3)/(n2+n3 - 2*k))
return(result)
}
F_break(30.85177,23.40327,2.941173,4,424,50)
[1] 19.93223
### 5 22 (p.316)                                                           ###
F_forecast<- function(ssr, ssr2,k,n2,n3){
result<- ((ssr - ssr2)/n3) / (ssr2/(n2-k))
return(result)
}
F_forecast(30.85177, 23.40327,4,424,50)
[1] 2.673447

###CUSUM Test
library(strucchange)
panel01<- lm(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY)
summary(panel01)
eq_cusum<- efp(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY,type = "OLS-CUSUM")
plot(eq_cusum)
plot(eq_cusum, alpha = 0.01, alt.boundary = TRUE)
## calculate corresponding test statistic
sctest(eq_cusum)
  OLS-based CUSUM test
data:  eq_cusum
S0 = 2.6647, p-value = 1.36e-06

eq_cusum2<- efp(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY,type = "Rec-CUSUM")
plot(eq_cusum2)

