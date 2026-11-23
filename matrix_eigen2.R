A_1<- matrix(c(2,1,2,3), nrow=2, ncol=2)
eigen(A_1)
eigen() decomposition
$values
[1] 4 1
$vectors
           [,1]       [,2]
[1,] -0.7071068 -0.8944272
[2,] -0.7071068  0.4472136

A_2<- matrix(c(-3,6,1,2), nrow=2, ncol=2)
eigen(A_2)
eigen() decomposition
$values
[1] -4  3
$vectors
           [,1]       [,2]
[1,] -0.7071068 -0.1643990
[2,]  0.7071068 -0.9863939

A_3<- matrix(c(1,1,-4,1), nrow=2, ncol=2)
eigen(A_3)
eigen() decomposition
$values
[1] 1+2i 1-2i
$vectors
                     [,1]                 [,2]
[1,] 0.8944272+0.0000000i 0.8944272+0.0000000i
[2,] 0.0000000-0.4472136i 0.0000000+0.4472136i

A_4<- matrix(c(1,2,-1,1), nrow=2, ncol=2)
eigen(A_4)
eigen() decomposition
$values
[1] 1+1.414214i 1-1.414214i
$vectors
                     [,1]                 [,2]
[1,] 0.0000000+0.5773503i 0.0000000-0.5773503i
[2,] 0.8164966+0.0000000i 0.8164966+0.0000000i

A_5<- matrix(c(1,1,1,-1), nrow=2, ncol=2)
eigen(A_5)
eigen() decomposition
$values
[1]  1.414214 -1.414214
$vectors
           [,1]       [,2]
[1,] -0.9238795  0.3826834
[2,] -0.3826834 -0.9238795

A_6<- matrix(c(3,2,-2,-2), nrow=2, ncol=2)
eigen(A_6)
eigen() decomposition
$values
[1]  2 -1
$vectors
          [,1]      [,2]
[1,] 0.8944272 0.4472136
[2,] 0.4472136 0.8944272
