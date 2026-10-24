Usage
filter(x, filter, method = c("convolution", "recursive"),
       sides = 2, circular = FALSE, init)

x <- 1:100
par(mfrow=c(2,2))
plot(x, type="l")
f1<- filter(x, rep(1, 3))
plot(f1, type="l")
f2<- filter(x, rep(1, 3), sides = 1)
plot(f2, type="l")
f3<- filter(x, rep(1, 3), sides = 1, circular = TRUE)
plot(f3)

str(presidents)
x<- presidents
par(mfrow=c(2,2))
plot(x, type="l")
plot(presidents)
f_p<- filter(presidents, rep(1, 3))
plot(f_p, type="l")

f3<- filter(x, rep(1, 3), sides = 1, circular = TRUE)
plot(f3, type="l")