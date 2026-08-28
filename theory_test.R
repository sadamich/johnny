### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Chi square test: Seite 240, 243,248,281,283,288
eyes<- 1:6
probs<- rep(1/length(eyes), length(eyes))
expected_value<- round(sum(eyes*probs),2)

n<- 10
values<- NULL
averages<- NULL

for (roll in 1:n){
values<- c(values, sample(x = eyes, size= 1,prob = probs))
averages<- c(averages, mean(values))
}
summary(averages)
hist(averages)

theory_value<- rep(3.5, 10)
x_sq<- sum((averages - theory_value)^2/theory_value)
x_sq
[1] 2.905082
pchisq(2.905082,9)
[1] 0.03206059

n<- 30
values<- NULL
averages<- NULL

for (roll in 1:n){
values<- c(values, sample(x = eyes, size= 1,prob = probs))
averages<- c(averages, mean(values))
}
summary(averages)
hist(averages)
chisq.test(averages)???

theory_value<- rep(3.5, 30)
x_sq<- sum((averages - theory_value)^2/theory_value)
x_sq
[1] 1.625982
pchisq(1.625982,29)
[1] 6.944626e-14


n<- 300
values<- NULL
averages<- NULL

for (roll in 1:n){
values<- c(values, sample(x = eyes, size= 1,prob = probs))
averages<- c(averages, mean(values))
}
summary(averages)
hist(averages)

theory_value<- rep(3.5, 300)
x_sq<- sum((averages - theory_value)^2/theory_value)
x_sq
[1] 8.311886
pchisq(8.311886,299)
[1] 1.071205e-171
