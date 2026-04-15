https://en.wikipedia.org/wiki/Cross_product
### Function crossprod                                                       ###
(z <- crossprod(1:4))    # = sum(1 + 2^2 + 3^2 + 4^2)
###       [,1]                                                               ###
### [1,]   30
drop(z)                  # scalar
### [1] 30                                                                   ###
x <- 1:4; names(x) <- letters[1:4]; x
### a b c d                                                                  ###
### 1 2 3 4                                                                  ### 
tcrossprod(as.matrix(x)) # is
###   a b  c  d                                                              ###
### a 1 2  3  4                                                              ###
### b 2 4  6  8                                                              ###
### c 3 6  9 12                                                              ###
### d 4 8 12 16                                                              ###

identical(tcrossprod(as.matrix(x)),
          crossprod(t(x)))
### [1] TRUE                                                                 ###
tcrossprod(x)            # no dimnames
###      [,1] [,2] [,3] [,4]                                                 ###
### [1,]    1    2    3    4                                                 ###
### [2,]    2    4    6    8                                                 ###
### [3,]    3    6    9   12                                                 ###
### [4,]    4    8   12   16                                                 ###
 
m <- matrix(1:6, 2,3) ; v <- 1:3; v2 <- 2:1
stopifnot(identical(tcrossprod(v, m), v %*% t(m)),
          identical(tcrossprod(v, m), crossprod(v, t(m))),
          identical(crossprod(m, v2), t(m) %*% v2))
###      [,1] [,2] [,3]                                                      ###
### [1,]    1    3    5                                                      ###
### [2,]    2    4    6                                                      ###
