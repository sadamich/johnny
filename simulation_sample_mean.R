https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Central-Limit-Theorem/Sampling-Error/index.html

pop<- c(1,2,3,4,5,6,7,8,9)
mean(pop)
### the population mean [1] 5                                                ###    
n<- 5

my_experiment<- NULL

for (i in 1:5){
my_sample<- sample(pop, n)
my_experiment<- c(my_experiment, mean(my_sample))
}
my_experiment
mean(my_experiment)
[1] 4.88
### the 30 times repeating 
my_experiment2<- NULL
for (i in 1:30){
my_sample2<- sample(pop, n)
my_experiment2<- c(my_experiment2, mean(my_sample2))
}
my_experiment2
mean(my_experiment2)
[1] 4.96

### the 100 times repeating
my_experiment3<- NULL
for (i in 1:100){
my_sample3<- sample(pop, n)
my_experiment3<- c(my_experiment3, mean(my_sample3))
}
my_experiment3
mean(my_experiment3)
[1] 5.004


my_experiment4<- NULL
for (i in 1:10000){
my_sample4<- sample(pop, n)
my_experiment4<- c(my_experiment4, mean(my_sample4))
}
my_experiment4
mean(my_experiment4)
[1] 5.00876


https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Central-Limit-Theorem/Sampling-Distribution/index.html

no_samples<- c(5, 30, 100, 3000, 10000)
for (i in 1:length(no_samples)){
my_samples<- rep(NA, no_samples[i])
for (j in 1:no_samples[i]){
my_samples[j]<- mean(sample(pop,n))
}
}
mean(my_samples)
