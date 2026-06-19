### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 2 11 (p.115)
xr201<- read.csv("xr201.csv", header = TRUE)
str(xr201)
attach(xr201)

### Problem (a)   constant or not inclusive                                ###
eq<- lm(RENDCYCO ~ RENDMARK)
summary(eq)
res<- resid(eq)

### Problem (b) the orthogonality condition                                ###
sum((RENDMARK-mean(RENDMARK))*res)
[1] -9.63396e-14    (2 11 OK)   the orthogonality
sum(res)
[1] -8.854029e-15   (2 11 OK)
sst<- sum (y^2) - 1/240*(sum(y))^2
sst
[1] 14726.25
sum(RENDCYCO^2)-1/240*(sum(RENDCYCO)^2)
ssr<- sum(res^2)
ssr
[1] 7311.877
1 - ssr/sst
[1] 0.5034802

### Without constant term                                                  ###
eq_nc<- lm(RENDCYCO ~ RENDMARK -1)
summary(eq_nc)
res_nc<- resid(eq_nc)
sum(res_nc)
[1] -104.3639
sum((RENDMARK-mean(RENDMARK))*res_nc)
[1] 84.41826

ssr_nc<- sum(res_nc^2)
ssr_nc
[1] 7358.578
1 - ssr_nc/sst
[1] 0.5003089    (Adjusted R-squared:  0.5003)

sse<- (1.1555)^2*( sum(RENDMARK^2) - 1/240*(sum(RENDMARK))^2)
sse
[1] 7217.812
sst<- sum(RENDCYCO^2) - 240*mean(RENDCYCO)^2
[1] 0.4901322???


### Problem (c)                                                            ###
plot(res, res_nc)
cor(res,res_nc)
[1] 0.9999099  (One to one relation) 
