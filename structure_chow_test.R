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


