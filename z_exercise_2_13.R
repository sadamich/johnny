### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 2 13 (p.115)

n<- 10
x<- c(101, 102,103,104,105,106,107,108,109,110)
e<- rnorm(10, 0,1)
y<- -100 + x +e
### Problem (a)                                                            ###
eq<- lm(y ~ x)
summary(eq)

### Problem (b)
confint(eq)
                   2.5 %     97.5 %
(Intercept) -151.3130528 -56.294318
x              0.5873473   1.487665
### Problem (c)

e2<- rnorm(10, 0,1)
y<- -100 + x +e2
eq2<- lm(y ~ x)
summary(eq2)
confint(eq2)
                   2.5 %     97.5 %
(Intercept) -129.0185877 -78.788931
x              0.7980289   1.273963

e3<- rnorm(10, 0,1)
y<- -100 + x +e3
eq3<- lm(y ~ x)
summary(eq3)
confint(eq3)
                   2.5 %     97.5 %
(Intercept) -125.0740471 -71.174775
x              0.7296101   1.240314
res3<- resid(eq3)
ssr3<- sum(res3^2)
1/(10-1)*ssr3
[1] 0.8992084

e4<- rnorm(10, 0,1)
y<- -100 + x +e4
eq4<- lm(y ~ x)
summary(eq4)
confint(eq4)
                   2.5 %     97.5 %
(Intercept) -137.2427921 -79.186685
x              0.8035612   1.353652

e5<- rnorm(10, 0,1)
y<- -100 + x +e5
eq5<- lm(y ~ x)
summary(eq5)
confint(eq5)
                  2.5 %      97.5 %
(Intercept) -97.7774888 -68.6290991
x             0.6984288   0.9746145

res5<- resid(eq5)
ssr5<- sum(res5^2)
1/(10-1)*ssr5
[1] 0.2629806


my_experiment <- NULL
for (i in 1:100) {
e<- rnorm(10, 0,1)
y<- -100 + x +e
eq<- lm(y ~ x)
z<- confint(eq)
  my_sample <- sample(z, size = 3)
  my_experiment <- c(my_experiment, z)
}
my_experiment
summary(my_experiment)

### Problem (d) the observations = 1000, trials = 100, sample size= ?