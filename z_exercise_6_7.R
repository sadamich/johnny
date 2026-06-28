### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
set.seed(44)
n<- 200
x<- 1:200
e<- dnorm(200,0,1)
y<- -10+0.1*x+e
plot(y)
### Problem (a) the censored model ###
### Vgl. Seite 117                                                         ###
y_c<- ifelse(y<0, 0, 1)
str(y_c)
y1<- y_c[y_c==1]
str(y1)
y0<- y_c[y_c==0]
str(y0)

### Problem (b) the odds ratio 
x_v<- c(60, 80, 100,120,140)
y_f<- function(x_v){
result<- -10+0.1*x_v+e
return(result)
}
y_f(x_v)
[1]                 -4 -2  0  2  4
### censored values  0  0  1  1  1                                        ###
### odds ratio 3/5* 5/2 = 3/2 = 1.5
x_v2<- c(55, 56, 57, 58, 59, 60, 61, 62, 63, 64)
y_f(x_v2)
[1]          -4.5 -4.4 -4.3 -4.2 -4.1 -4.0 -3.9 -3.8 -3.7 -3.6
### Censored   0   0    0    0    0   0    0    0    0      0
### odds ration 0/10 * 10/10
x_v3<- c(65, 66, 67, 68, 69, 70, 71, 72, 73, 74)
y_f(x_v3)
 [1] -3.5 -3.4 -3.3 -3.2 -3.1 -3.0 -2.9 -2.8 -2.7 -2.6

x_v_s<- 1:200
y_f(x_v_s)
0 has 99 observations
1 has 101 obervations 

P(y=1) = 101/200
P(y=0) = 99/200
P(y=1)/P(y=0) = 101/200* 200/99
101/99
[1] 1.020202
### Problem (c) the regression and the odds ratio

### Problem (d) the probit model

### Problem (e) the logit model 

### Problem (f) the logit model and the odds ratio

