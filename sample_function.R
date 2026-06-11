pop<- 1:1000
n<- 30
sample1<- sample(pop, n)
sample1
mean(sample1)
[1] 494.0333
sd(sample1)
[1] 301.7686
t<- function(mean, sd, n){
result<- mean/(sd/sqrt(n))
return(result)
}
t(494.0333,301.7686,30)
[1] 8.96691      (t value)

sample2<- sample(pop,n)
sample2
mean(sample2)
[1] 513.1
sd(sample2)
[1] 244.6274
t(513.1,244.6274,30)
[1] 11.48835     (t value)

sample3<- sample(pop,n)
mean(sample3)
[1] 501.5667
sd(sample3)
[1] 249.9657
t(501.5667,249.9657,30)
[1] 10.99028     (t value) 

sample4<- sample(pop,n)
mean(sample4)
[1] 508.6
sd(sample4)
[1] 279.0932
t(508.6,279.0932,30)
[1] 9.981314     (t value)

for (i in 1:30)


t_statistic<- c( 8.96691,11.48835,10.99028, 9.981314)

plot(t_statistic, type="l")


http://127.0.0.1:31100/library/base/html/sample.html
### function sample                                                          ###
x <- 1:12
# a random permutation
sample(x)
# bootstrap resampling -- only if length(x) > 1 !
sample(x, replace = TRUE)
# 100 Bernoulli trials
sample(c(0,1), 100, replace = TRUE)
## More careful bootstrapping --  Consider this when using sample()
## programmatically (i.e., in your function or simulation)!
# sample()'s surprise -- example
x <- 1:10
    sample(x[x >  8]) # length 2
    sample(x[x >  9]) # oops -- length 10!
    sample(x[x > 10]) # length 0
## safer version:
resample <- function(x, ...) x[sample.int(length(x), ...)]
resample(x[x >  8]) # length 2
resample(x[x >  9]) # length 1
resample(x[x > 10]) # length 0

## R 3.0.0 and later
sample.int(1e10, 12, replace = TRUE)
sample.int(1e10, 12) # not that there is much chance of duplicates
 