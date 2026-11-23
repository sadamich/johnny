### 3 8 Panel data
y <- rbind(y1 - mean(y1),y2 - mean(y2),y3 - mean(y3))
x <- rbind(x1 - mean(x1),x2 - mean(x2),x3 - mean(x3))
res <- gmm(y~x,h)
y <- rbind(y1,y2,y3)
x <- rbind(x1,x2,x3)
res <- gmm(y~x,h)

### Sandwich 
gt <- g(t0, x)
V <- kernHAC(lm(gt~1), sandwich = FALSE)
W <- solve(V)