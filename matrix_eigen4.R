C_1<- matrix(c(2,1,-1,0), nrow=2, ncol=2)
eigen(C_1)
eigen() decomposition
$values
[1] 1 1
$vectors
          [,1]      [,2]
[1,] 0.7071068 0.7071068
[2,] 0.7071068 0.7071068

C_2<- matrix(c(1,2,0,-1), nrow=2, ncol=2)
eigen(C_2)
eigen() decomposition
$values
[1] -1  1
$vectors
     [,1]      [,2]
[1,]    0 0.7071068
[2,]    1 0.7071068


C_3<- matrix(c(3,1,-4,-1), nrow=2, ncol=2)
eigen(C_3)
eigen() decomposition
$values
[1] 1 1
$vectors
          [,1]      [,2]
[1,] 0.8944272 0.8944272
[2,] 0.4472136 0.4472136
