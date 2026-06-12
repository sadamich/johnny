https://search.r-project.org/R/refmans/stats/html/TDist.html
https://de.wikipedia.org/wiki/Studentsche_t-Verteilung
Usage
dt(x, df, ncp, log = FALSE)
pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
rt(n, df, ncp)

### CLT t(v) -> N(0,1)                                                     ###

### Wendepunkt   x= +-sqrt(n/(n+2))                                        ###
w<- function(n){
result<- sqrt(n/(n+2))
return(result)
}

n<- 1:100
ex_w<- w(n)
plot(ex_w, type="l")

w2<- function(n){
result<- -sqrt(n/(n+2))
return(result)
}
n<- 1:100
ex_w2<- w2(n)
plot(ex_w2, type="l")