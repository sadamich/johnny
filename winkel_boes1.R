https://cran.r-project.org/web/packages/AER/refman/AER.html#WinkelmannBoes2009
library(AER)
## data
data("GSS7402", package = "AER")
str(GSS7402)
attach(GSS7402)
## completed fertility subset
gss40 <- subset(GSS7402, age >= 40)

## Chapter 1
hist(kids)
summary(kids)
## Table 1.1
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.000   2.000   2.076   3.000   8.000 
gss_kids <- table(gss40$kids)
cbind(absolute = gss_kids,
  relative = round(prop.table(gss_kids) * 100, digits = 2))



## Chapter 4, Example 4.1
gss40$nokids01 <- as.numeric(gss40$nokids) - 1
nokids_lm3 <- lm(nokids01 ~ trend + education + ethnicity + siblings, data = gss40)
coeftest(nokids_lm3, vcov = sandwich)

## Example 4.3
## Table 4.1
nokids_l1 <- glm(nokids ~ 1, data = gss40, family = binomial(link = "logit"))
nokids_l3 <- glm(nokids ~ trend + education + ethnicity + siblings,
  data = gss40, family = binomial(link = "logit"))
lrtest(nokids_p3)
lrtest(nokids_l3)