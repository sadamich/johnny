https://en.wikipedia.org/wiki/Robust_statistics

Tukey's biweight (also known as bisquare) function behaves in a similar way
to the squared error function at first, but for larger errors, the function 
tapers off.


Description
Functions of the distribution of the studentized range, 


Usage
ptukey(q, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE)
qtukey(p, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE)


if(interactive())
  curve(ptukey(x, nm = 6, df = 5), from = -1, to = 8, n = 101)
(ptt <- ptukey(0:10, 2, df =  5))
(qtt <- qtukey(.95, 2, df =  2:11))
## The precision may be not much more than about 8 digits:
summary(abs(.95 - ptukey(qtt, 2, df = 2:11)))