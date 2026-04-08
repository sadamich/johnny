### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr530<- read.csv("xr530.csv", header =TRUE)
str(xr530)
attach(xr530)
detach(xr530)
### Problem a (p. 434)                                                     ###
eq<- lm(RENDCYCO ~ RENDMARK)
res<- resid(eq)
RENDMARK_sq<- RENDMARK^2
eq_res<- lm(res^2 ~ RENDMARK+RENDMARK_sq)
summary(eq_res)
plot(res, RENDMARK)
plot(res^2, RENDMARK^2)
240*0.03065 = 7.356
acf(res)
res<- ts(res,freq=12, start=1980)
plot(res)
eq_sc<- lm(res[2:195] ~ RENDMARK[2:195]+ res[1:194])
summary(eq_sc)
194*0.0001684=  0.0326696
eq_sc2<- lm(res[3:195] ~ RENDMARK[3:195]+ res[2:194]+res[1:193])
summary(eq_sc2)
193*0.0006863=  0.1324559
### Problem b                                                                ###
eq_n<- lm(RENDNCCO ~ RENDMARK)
res_n<- resid(eq_n)
res_n<- ts(res_n,freq = 12, start=1980)
plot(res_n)
RENDMARK_sq<- RENDMARK^2
eq_res_n<- lm(res_n^2 ~ RENDMARK+RENDMARK_sq)
summary(eq_res_n)
plot(res_n, RENDMARK)
plot(res_n^2, RENDMARK^2)
acf(res_n)
eq_sc_n<- lm(res_n[2:195] ~ RENDMARK[2:195]+ res_n[1:194])
summary(eq_sc_n)
194* 0.00691 = 1.34054
eq_sc2_n<- lm(res_n[3:195] ~ RENDMARK[3:195]+ res_n[2:194]+res_n[1:193])
summary(eq_sc2_n)
193* 0.01592= 3.07256