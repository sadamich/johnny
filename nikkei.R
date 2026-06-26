https://data.ecb.europa.eu/data/datasets/FM/FM.M.JP.JPY.DS.EI.JAPDOWA.HSTA


nikkei<- read.csv("nikkei.csv", header=TRUE)
str(nikkei)
attach(nikkei)
head(NIKKEI,5)
str(NIKKEI)
x<- ts(NIKKEI, freq= 12, start=1973)
plot(x, main="Time series", ylab="NIKKEI")