### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 6 2 (p.451): Example 6 3 (p.457)                               ###
xm601<- read.csv("xm601.csv", header = TRUE)
attach(xm601)
str(xm601)

xm601_no<- na.omit(xm601)
attach(xm601_no)
AGE_2<- AGE^2/100
panel01<- lm(RESPONSE~GENDER+ACTIVITY+AGE+AGE_2)
summary(panel01)
### Logit model                                                            ###
panel02<- glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, 
family = binomial)
summary(panel02)
library(sandwich)
library(lmtest)
z<-coeftest(panel02, vcov=sandwich)
summary(panel02, vcov=sandwich)
lrtest(panel02,"AGE_2","AGE")
Likelihood ratio test
Model 1: RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2
Model 2: RESPONSE ~ GENDER + ACTIVITY + AGE
Model 3: RESPONSE ~ GENDER + ACTIVITY
  #Df  LogLik Df  Chisq Pr(>Chisq)  
1   5 -601.86                       
2   4 -603.96 -1 4.2003    0.04042 *
3   3 -603.99 -1 0.0468    0.82877  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.

waldtest(panel02, vcov = sandwich)
Wald test
Model 1: RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2
Model 2: RESPONSE ~ 1
  Res.Df Df      F    Pr(>F)    
1    920                        
2    924 -4 16.896 2.239e-13 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
lmtest(panel02, vcov = sandwich)
### Standardized residuals:Panel 1 (p.458)
fit_logit<- fitted(panel02)
res_s<- (RESPONSE - fit_logit)/sqrt(fit_logit*(1-fit_logit))
summary(res_s)
 Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-1.7856934 -1.0457664  0.5602327  0.0007331  0.9097641  2.1233691 
plot(res_s)

w_lm<- fit_logit/sqrt(fit_logit*(1-fit_logit))
eq_lm<- lm(res_s ~ LOGINV -1 , weight=w_lm )
summary(eq_lm)
eq_sig<- lm(res_s~ exp(LOGINV)-1)
summary(eq_sig)
 0.3998*925


### The empirical probability (from sample)                                ###
response<- RESPONSE[RESPONSE==1]
str(response)
int [1:470] 1 1 1 1 1 1 1 1 1 1 
p<- 470/925
random_hit<- p^2+(1-p)^2
random_hit
[1] 0.5001315
z<- function(h, q, n){
result <- (n*h - n*q)/(sqrt(n*q*(1-q)))
return(result)
}
z(0.6162162, 0.5001315,925)
[1] 7.061157

fit_logit<- fitted(panel02)
y_f<- ifelse(fit_logit >1/2, 1,0)
y_f
y_f1<- y_f[y_f==1]
str(y_f1)
 Named num [1:633] 1 1 1 1 1 1 1 1 1 1 ...
 - attr(*, "names")= chr [1:633] "2" "3" "4" "7" ...
w<- ifelse(RESPONSE== y_f, 1, 0)
w_1<- w[w==1]
str(w_1)
 Named num [1:570] 1 1 1 1 1 1 1 1 1 1 ...
 - attr(*, "names")= chr [1:570] "2" "3" "4" "7" ...
### The hit rates : Panel 2 (p.458)                                        ###
1/925*sum(w)
[1] 0.6162162   (h: the hit rates) 