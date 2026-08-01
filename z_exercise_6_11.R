### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 11 (p. 528) Marketing 

xm601<- read.csv("xm601.csv", header=TRUE)
str(xm601)
attach(xm601)
AGE_2<- AGE^2
### Problem (a) The Logit model
eq_logit<- glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, 
family = binomial)
summary(eq_logit)
p_fit<- fitted(eq_logit)
plot(p_fit)
plot(GENDER[GENDER==1],p_fit[GENDER==1])

f1<- function(AGE){
result<- -2.4883577+0.9536944*1+ 0.9137479*1+ 0.0699453*AGE--0.0006869*AGE^2
return(result)
} 

f1(AGE)
f1_no<- na.omit(f1)

f2<- function(AGE){
result<- -2.4883577+0.9536944*1+ 0.0699453*AGE--0.0006869*AGE^2
return(result)
} 
f2(AGE)
f2_no<- na.omit(f2)
plot(f2)
plot(f1_no,f2_no)
Error in seq.int(from, to, length.out = n) : 
  'from' must be a finite number
y<- RESPONSE
y_active<- RESPONSE[GENDER==1]and[ACTIVITY==1]
### Problem (b) The Probit model
eq_probit<- glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, 
family = binomial(link = "probit"))
summary(eq_probit)
p_fit_pro<- fitted(eq_probit)
### Problem (c) 

### Problem (d) Random sampling





detach(xm601)