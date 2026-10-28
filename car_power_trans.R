https://cran.r-project.org/web/packages/car/refman/car.html#powerTransform
library(car)

# Box Cox Method, univariate
attach(Wool)
str(Wool)
'data.frame':   27 obs. of  4 variables:
 $ len   : int  250 250 250 250 250 250 250 250 250 300 ...
 $ amp   : int  8 8 8 9 9 9 10 10 10 8 ...
 $ load  : int  40 45 50 40 45 50 40 45 50 40 ...
 $ cycles: int  674 370 292 338 266 210 170 118 90 1414 ...
summary(p1 <- powerTransform(cycles ~ len + amp + load, Wool))
# fit linear model with transformed response:
coef(p1, round=TRUE)
summary(m1 <- lm(bcPower(cycles, p1$roundlam) ~ len + amp + load, Wool))

# Multivariate Box Cox uses Highway1 data
summary(powerTransform(cbind(len, adt, trks, sigs1) ~ 1, Highway1))

# Multivariate transformation to normality within levels of 'htype'
summary(a3 <- powerTransform(cbind(len, adt, trks, sigs1) ~ htype, Highway1))

# test lambda = (0 0 0 -1)
testTransform(a3, c(0, 0, 0, -1))

# save the rounded transformed values, plot them with a separate
# color for each highway type
transformedY <- bcPower(with(Highway1, cbind(len, adt, trks, sigs1)),
                        coef(a3, round=TRUE))
## Not run:  # generates a smoother warning
scatterplotMatrix( ~ transformedY|htype, Highway1) 

## End(Not run)

# With negative responses, use the bcnPower family
m2 <- lm(I1L1 ~ pool, LoBD)
summary(p2 <- powerTransform(m2, family="bcnPower"))
testTransform(p2, .5)
summary(powerTransform(update(m2, cbind(LoBD$I1L2, LoBD$I1L1) ~ .), family="bcnPower"))

## Not run:  # takes a few seconds:
  # multivariate bcnPower, with 8 responses
  summary(powerTransform(update(m2, as.matrix(LoBD[, -1]) ~ .), family="bcnPower"))
  # multivariate bcnPower, fit with one iteration using starting values as estimates
  summary(powerTransform(update(m2, as.matrix(LoBD[, -1]) ~ .), family="bcnPower", itmax=1))

## End(Not run)

# mixed effects model
## Not run:  # uses the lme4 package
  data <- reshape(LoBD[1:20, ], varying=names(LoBD)[-1], direction="long", v.names="y")
  names(data) <- c("pool", "assay", "y", "id")
  data$assay <- factor(data$assay)
  require(lme4)
  m2 <- lmer(y ~ pool + (1|assay), data)
  summary(l2 <- powerTransform(m2, family="bcnPower", verbose=TRUE))
