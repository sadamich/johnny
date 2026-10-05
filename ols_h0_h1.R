### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
### Panel 1 - Panel 6 (p.138)
h0<- lm(LOGSAL~EDUC)
summary(h0)
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 9.062102   0.062738   144.4   <2e-16 ***
EDUC        0.095963   0.004548    21.1   <2e-16 ***
resh0<- resid(h0)
h1<- lm(LOGSAL~EDUC+LOGSALBEGIN)
summary(h1)
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.646916   0.274598   5.998 3.99e-09 ***
EDUC        0.023122   0.003894   5.938 5.59e-09 ***
LOGSALBEGIN 0.868505   0.031835  27.282  < 2e-16 ***
resh1<- resid(h1)

eq3<- lm(resh1~EDUC+LOGSALBEGIN)
summary(eq3)
              Estimate Std. Error t value Pr(>|t|)
(Intercept) -2.597e-16  2.746e-01       0        1
EDUC        -1.863e-18  3.894e-03       0        1
LOGSALBEGIN  2.485e-17  3.183e-02       0        1

eq4<- lm(resh0~EDUC)
summary(eq4)
              Estimate Std. Error t value Pr(>|t|)
(Intercept)  1.001e-17  6.274e-02       0        1
EDUC        -5.530e-19  4.548e-03       0        1

eq5<- lm(resh0~LOGSALBEGIN)
summary(eq5)
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -4.44913    0.29569  -15.05   <2e-16 ***
LOGSALBEGIN  0.46012    0.03056   15.06   <2e-16 ***

eq6<- lm(LOGSALBEGIN~EDUC)
summary(eq6)
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 8.537878   0.056531  151.03   <2e-16 ***
EDUC        0.083869   0.004098   20.47   <2e-16 ***

### b_r = b_1 + P * b_2                                                  ###
b_1<- c(1.647, 0.023)
p<- c(8.538, 0.084)
b_r<- b_1 + p*0.869
b_r
[1] 9.066522 0.095996

