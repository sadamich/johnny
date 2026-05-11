### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 1 14 (p.74)

Usage
dt(x, df, ncp, log = FALSE)
pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
rt(n, df, ncp)

### Problem (a)                                                            ###
set.seed(13)
y<- rt(10,3)
y_mean<- mean(y)
y_sd<- sd(y)

y_mean + 2*y_sd/sqrt(10)
[1] 0.8014445
y_mean - 2*y_sd/sqrt(10)
[1] -0.8996569

### Problem (b)
set.seed(15)
y<- rt(10,3)
y_mean<- mean(y)
y_sd<- sd(y)
y_mean + 2*y_sd/sqrt(10)
[1] 1.852381
y_mean - 2*y_sd/sqrt(10)
[1] -0.1064735
set.seed(17)
y<- rt(10,3)
y_mean<- mean(y)
y_sd<- sd(y)

y_mean + 2*y_sd/sqrt(10)
[1] 0.7119812
y_mean - 2*y_sd/sqrt(10)
[1] -0.268698

### Problem (c)
n = 100,n= 1000

### Problem (d)

population <- rt(10,3)
my_experiment <- NULL
for (i in 1:30) {
  my_sample <- sample(population, size = 10)
  my_experiment <- c(my_experiment, mean(my_sample))
  cat(sprintf("Sample number %s has a mean of %s.\n", i, round(mean(my_sample), 2)))
}



