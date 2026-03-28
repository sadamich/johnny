### function sort                                                          ###

x <- swiss$Education[1:25]
plot(x)
hist(x)
x
 [1] 12  9  5  7 15  7  7  8  7 13  6 12  7 12  5
      2  8 28 20  9 10  3 12  6  1

x; sort(x); sort(x, partial = c(10, 15))

 [1] 12  9  5  7 15  7  7  8  7 13  6 12  7 12  5  2  8 28 20  9 10  3 12  6  1 (Original)
 [1]  1  2  3  5  5  6  6  7  7  7  7  7  8  8  9  9 10 12 12 12 12 13 15 20 28 (Increasing)
 [1]  3  2  5  5  1  6  6  7  7  7  7  8  7  8  9  9 10 12 12 12 12 20 28 13 15


x; sort(x); sort(x, partial = c(5, 15))
[1] 12  9  5  7 15  7  7  8  7 13  6 12  7 12  5  2  8 28 20  9 10  3 12  6  1 (Original)
[1]  1  2  3  5  5  6  6  7  7  7  7  7  8  8  9  9 10 12 12 12 12 13 15 20 28 (Increading)
[1]  1  2  3  5  5  6  7  7  6  7  7  7  8  8  9  9 12 10 12 12 13 12 20 28 15


## illustrate 'stable' sorting (of ties):
sort(c(10:3, 2:12), method = "shell", index.return = TRUE) # is stable
## $x : 2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 12
## $ix: 9  8 10  7 11  6 12  5 13  4 14  3 15  2 16  1 17 18 19
sort(c(10:3, 2:12), method = "quick", index.return = TRUE) # is not
## $x : 2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 12
## $ix: 9 10  8  7 11  6 12  5 13  4 14  3 15 16  2 17  1 18 19

x <- c(1:3, 3:5, 10)
is.unsorted(x)                  # FALSE: is sorted
is.unsorted(x, strictly = TRUE) # TRUE : is not (and cannot be)
                                # sorted strictly


## Small speed comparison simulation:
N <- 2000
Sim <- 20
rep <- 1000 # << adjust to your CPU
c1 <- c2 <- numeric(Sim)
for(is in seq_len(Sim)){
  x <- rnorm(N)
  c1[is] <- system.time(for(i in 1:rep) sort(x, method = "shell"))[1]
  c2[is] <- system.time(for(i in 1:rep) sort(x, method = "quick"))[1]
  stopifnot(sort(x, method = "shell") == sort(x, method = "quick"))
}
rbind(ShellSort = c1, QuickSort = c2)

        [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
ShellSort 0.16 0.17 0.16 0.17 0.17 0.15 0.18 0.14 0.17  0.16  0.17  0.17  0.17
QuickSort 0.12 0.11 0.13 0.12 0.14 0.12 0.12 0.11 0.12  0.13  0.13  0.12  0.14
          [,14] [,15] [,16] [,17] [,18] [,19] [,20]
ShellSort  0.17  0.17  0.15  0.18  0.17  0.17  0.16
QuickSort  0.12  0.14  0.13  0.13  0.10  0.13  0.12

cat("Speedup factor of quick sort():\n")

summary({qq <- c1 / c2; qq[is.finite(qq)]})
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.154   1.231   1.321   1.343   1.417   1.700 

## A larger test
x <- rnorm(1e7)
system.time(x1 <- sort(x, method = "shell"))
system.time(x2 <- sort(x, method = "quick"))
system.time(x3 <- sort(x, method = "radix"))
stopifnot(identical(x1, x2))
stopifnot(identical(x1, x3))


