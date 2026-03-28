### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### 5 2 3 Non parametrich estimation (p.289)                               ###                            
w<- function(d){
result<- (1-d^3)^3
return(result)
}

w(0)
[1] 1
w(0.2)
[1] 0.9761915
w(0.4)
[1] 0.8200259
w(0.6)
[1] 0.4818903
w(0.8)
[1] 0.1162143
w(1)
[1] 0

curve(w, 0, 1, lty=2, ylim=c(0, 1), xlab = "d", ylab="Weigth")

### Example 5 3 (p. 293)                                                   ###
x<- runif(200, 0, 2.5)
e<- rnorm(200, 0, 0.04)
y<- sin(x)+e
plot(x,y)