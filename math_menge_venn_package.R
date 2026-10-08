

library(eulerr)

# Fit a diagram with circles
combo <- c(A = 2, B = 2, C = 2, "A&B" = 1, "A&C" = 1, "B&C" = 1)
fit1 <- euler(combo)
plot(fit1)
# Investigate the fit
fit1

po<- c(A = 1, B= 2, C= 3, "A&B" =3 , "A&C"=4)
v<- euler(po)
plot(v)
# Refit using ellipses instead
fit2 <- euler(combo, shape = "ellipse")
# Investigate the fit again (which is now exact)
fit2
# Plot it
plot(fit2)

# A set with no perfect solution
euler(c(
  "a" = 3491, "b" = 3409, "c" = 3503,
  "a&b" = 120, "a&c" = 114, "b&c" = 132,
  "a&b&c" = 50
))


# Using grouping via the 'by' argument through the data.frame method
z<- euler(fruits, by = list(sex, age))
plot(z)
str(fruits)
'data.frame':   100 obs. of  5 variables:
 $ banana: logi  FALSE FALSE TRUE TRUE FALSE TRUE ...
 $ apple : logi  FALSE FALSE TRUE FALSE FALSE TRUE ...
 $ orange: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
 $ sex   : Factor w/ 2 levels "female","male": 1 2 2 2 2 1 2 1 2 2 ...
 $ age   : Factor w/ 2 levels "adult","child": 1 2 1 1 1 1 2 1 2 1 ...




# Using the matrix method
euler(organisms)

# Using weights
euler(organisms, weights = c(10, 20, 5, 4, 8, 9, 2))

# The table method
euler(pain, factor_names = FALSE)

# A euler diagram from a list of sample spaces (the list method)
euler(plants[c("erigenia", "solanum", "cynodon")])