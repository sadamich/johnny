https://www.geo.fu-berlin.de/en/v/soga-r/Advanced-statistics/Multivariate-approaches/Principal-Component-Analysis/PCA-the-basics/Data-preparation/index.html

food <- read.csv("food-texture.csv")
str(food)
'data.frame':   50 obs. of  6 variables:
 $ X       : chr  "B110" "B136" "B171" "B192" ...
 $ Oil     : num  16.5 17.7 16.2 16.7 16.3 19.1 18.4 17.5 15.7 16.4 ...
 $ Density : int  2955 2660 2870 2920 2975 2790 2750 2770 2955 2945 ...
 $ Crispy  : int  10 14 12 10 11 13 13 10 11 11 ...
 $ Fracture: int  23 9 17 31 26 16 17 26 23 24 ...
 $ Hardness: int  97 139 143 95 143 189 114 63 123 132 ...
pca_toy <- food[, c("Oil", "Density")]
plot(pca_toy)
# Function for centering a vector
center <- function(v) {
  v - mean(v)
}
# Function for scaling a vector
scale <- function(v) {
  v / sd(v)
}
## save helper functions for later usage
save(center, scale, file = "helper_functions_30300.RData")
# Apply use defined function on each column of the data set
pca_toy_centered <- apply(pca_toy, MARGIN = 2, FUN = center)
pca_toy_scaled <- apply(pca_toy_centered, MARGIN = 2, FUN = scale)
## Plotting ###
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))
###############
# scatterplot 1
plot(pca_toy, main = "Raw data")
# calulate mean for visualization
data_mean <- apply(pca_toy, 2, mean)
points(data_mean[1], data_mean[2], col = "red", pch = 16) # mark mean
# boxplot 1
boxplot(pca_toy, main = "Raw data")
###############
# scatterplot 2
plot(pca_toy_centered, main = "Centered data")
# calulate mean for visualization
data_mean <- apply(pca_toy_centered, 2, mean)
points(data_mean[1], data_mean[2], col = "red", pch = 16) # mark mean
# boxplot 2
boxplot(pca_toy_centered, main = "Centered data")
###############
# scatterplot 3
plot(pca_toy_scaled, main = "Scaled and centered data")
# calulate mean for visualization
data_mean <- apply(pca_toy_scaled, 2, mean)
points(data_mean[1], data_mean[2], col = "red", pch = 16) # mark mean
# boxplot 3
boxplot(pca_toy_scaled, main = "Scaled and centered data")
# assign propper variable name to pre-processed data set
pca_toy_data <- pca_toy_scaled
# save for later usage
save(pca_toy_data, file = "pca_food_toy_30300.RData")


# load data from previous section
load("pca_food_toy_30300.RData")
# Compute eigenvalues and eigenvectors of covariance matrix (= correlation matrix) of our scaled variables
pca_toy_cov <- cov(pca_toy_data)
pca_toy_eigen <- eigen(pca_toy_cov)
# Access the eigenvectors
rownames(pca_toy_eigen$vectors) <- c("Oil", "Density") # label output matrix
colnames(pca_toy_eigen$vectors) <- c("PC1", "PC2") # label output matrix
pca_toy_eigen$vectors
 PC1        PC2
Oil     -0.7071068 -0.7071068
Density  0.7071068 -0.7071068
pca_toy_eigen$values
[1] 1.750024 0.249976

# first principal component vector slope
s1 <- pca_toy_eigen$vectors[1, 1] / pca_toy_eigen$vectors[2, 1] # PC1
plot(pca_toy_data,
     asp = TRUE,
     pch = 16,
     xlab = "Oil",
     ylab = "Density",
     main = "First principal component")
abline(a = 0, b = s1, col = "red")
legend("topleft", legend = "PC1", col = "red", lty = 1)
# principal component vector slopes
s1 <- pca_toy_eigen$vectors[1, 1] / pca_toy_eigen$vectors[2, 1] # PC1
s2 <- pca_toy_eigen$vectors[1, 2] / pca_toy_eigen$vectors[2, 2] # PC2
plot(pca_toy_data,
     asp = TRUE,
     pch = 16,
     xlab = "Oil",
     ylab = "Density",
     main = "First principal component")
abline(a = 0, b = s1, col = "red")
abline(a = 0, b = s2, col = "blue")
legend("topleft", legend = c("PC1", "PC2"), col = c("red", "blue"), lty = 1)

food <- read.csv("food-texture.csv")
# load helper functions from previous sections
load("helper_functions_30300.RData")
library(dplyr)
# center and scale data set
# useage of the pipe operator %>% provided by the dplyr package
food_pca <- food[, 2:ncol(food)] %>%
  apply(MARGIN = 2, FUN = center) %>%
  apply(MARGIN = 2, FUN = scale)
dim(food_pca)
save(food_pca, file = "pca_food_30300.RData")
food_pca_eigen <- eigen(cov(food_pca))
food_pca_eigen$values
[1] 3.0312132 1.2957058 0.3100493 0.2419201 0.1211116
all.equal(sum(food_pca_eigen$values), sum(apply(food_pca, MARGIN = 2, FUN = var)))
food_pca_ve <- food_pca_eigen$values / sum(food_pca_eigen$values)
food_pca_ve
[1] 0.60624263 0.25914115 0.06200987 0.04838402 0.02422233
par(mfrow = c(1, 2), mar = c(4, 5, 3, 1))
plot(food_pca_ve,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1),
     type = "b",
     main = "Scree plot")

plot(cumsum(food_pca_ve),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of\nVariance Explained",
     ylim = c(0, 1),
     type = "b",
     main = "Scree plot")
cumsum(food_pca_eigen$values / sum(food_pca_eigen$values))
[1] 0.6062426 0.8653838 0.9273937 0.9757777 1.0000000
food_pca_eigen$values[food_pca_eigen$values >= 1]
[1] 3.031213 1.295706

library("psych")
library("REdaS")
bart_spher(food_pca)
 Bartlett's Test of Sphericity
Call: bart_spher(x = food_pca)

     X2 = 154.994
     df = 10
p-value < 2.22e-16
food_m<- food[, 2:6]
bart_spher(food_m)
   Bartlett's Test of Sphericity

Call: bart_spher(x = food_m)

     X2 = 154.994
     df = 10
p-value < 2.22e-16

kmos_food<- KMOS(food_pca)
print(kmos_food, stats = "KMO")

kmos_food<- KMOS(food_m)
print(kmos_food, stats = "KMO")
r-Meyer-Olkin Statistic
Call: KMOS(x = food_m)
KMO-Criterion: 0.7088777


print(kmos_food, stats = "MSA", sort = TRUE, digits = 3, show = 1:5)
Kaiser-Meyer-Olkin Statistics
Call: KMOS(x = food_m)
Measures of Sampling Adequacy (MSA):
Hardness   Crispy  Density Fracture      Oil 
   0.428    0.674    0.706    0.788    0.818 
VSS.scree(food_m)
VSS.scree(food_pca)

pca.food <- principal(food_m, 5, rotate = "none")
pca.food$criteria <- NULL
pca.food
Principal Components Analysis
Call: principal(r = food_m, nfactors = 5, rotate = "none")
Standardized loadings (pattern matrix) based upon correlation matrix
           PC1   PC2   PC3   PC4   PC5 h2      u2 com
Oil       0.80 -0.42  0.37  0.23  0.00  1 5.6e-16 2.2
Density  -0.83  0.41  0.01  0.35  0.12  1 2.2e-16 1.9
Crispy    0.93  0.22 -0.10 -0.07  0.28  1 1.0e-15 1.3
Fracture -0.88 -0.25  0.30 -0.22  0.15  1 1.1e-15 1.6
Hardness  0.27  0.92  0.27 -0.10 -0.08  1 1.1e-16 1.4

                       PC1  PC2  PC3  PC4  PC5
SS loadings           3.03 1.30 0.31 0.24 0.12
Proportion Var        0.61 0.26 0.06 0.05 0.02
Cumulative Var        0.61 0.87 0.93 0.98 1.00
Proportion Explained  0.61 0.26 0.06 0.05 0.02
Cumulative Proportion 0.61 0.87 0.93 0.98 1.00

Mean item complexity =  1.7
Fit based upon off diagonal values = 1> 


pca.foodr <- principal(food_m, 5)
pca.foodr$criteria <- NULL
print(pca.foodr, cut = 0.5, sort = TRUE, digits = 2)
Principal Components Analysis
Call: principal(r = food_m, nfactors = 5)
Standardized loadings (pattern matrix) based upon correlation matrix
         item   RC1   RC3   RC2   RC4   RC5 h2      u2 com
Fracture    4  0.90                          1 1.1e-15 1.5
Crispy      3 -0.61                    0.56  1 1.0e-15 3.7
Oil         1        0.89                    1 5.6e-16 1.5
Hardness    5              0.97              1 1.1e-16 1.1
Density     2                    0.85        1 2.2e-16 1.8

                       RC1  RC3  RC2  RC4  RC5
SS loadings           1.38 1.11 1.10 1.04 0.36
Proportion Var        0.28 0.22 0.22 0.21 0.07
Cumulative Var        0.28 0.50 0.72 0.93 1.00
Proportion Explained  0.28 0.22 0.22 0.21 0.07
Cumulative Proportion 0.28 0.50 0.72 0.93 1.00

Mean item complexity =  1.9
Fit based upon off diagonal values = 1


fa.diagram(pca.foodr, cut = 0.5, cex = 0.8, rsize= 0.5, main= "")


pca.food2 <- principal(food_m, 2)
pca.food2$criteria <- NULL
print(pca.food2, cut = 0.5, sort = TRUE, digits = 2)
Principal Components Analysis
Call: principal(r = food_m, nfactors = 2)
Standardized loadings (pattern matrix) based upon correlation matrix
         item   RC1   RC2   h2   u2 com
Density     2  0.93       0.86 0.14 1.0
Oil         1 -0.90       0.81 0.19 1.0
Crispy      3 -0.77  0.57 0.91 0.09 1.8
Fracture    4  0.71 -0.57 0.83 0.17 1.9
Hardness    5        0.95 0.91 0.09 1.0

                       RC1  RC2
SS loadings           2.77 1.56
Proportion Var        0.55 0.31
Cumulative Var        0.55 0.87
Proportion Explained  0.64 0.36
Cumulative Proportion 0.64 1.00

Mean item complexity =  1.4
Fit based upon off diagonal values = 0.99>


fa.diagram(pca.food2, cut = 0.5, cex = 0.8, rsize= 0.5, main= "")


pca.food2 <- principal(food_pca, 2)
pca.food2$criteria <- NULL
pca.food2


pca.food3 <- principal(food_pca, 5, rotate ="none" )
pca.food3$criteria <- NULL
pca.food3
Principal Components Analysis
Call: principal(r = food_pca, nfactors = 5, rotate = "none")
Standardized loadings (pattern matrix) based upon correlation matrix
           PC1   PC2   PC3   PC4   PC5 h2       u2 com
Oil       0.80 -0.42  0.37  0.23  0.00  1  1.0e-15 2.2
Density  -0.83  0.41  0.01  0.35  0.12  1 -4.4e-16 1.9
Crispy    0.93  0.22 -0.10 -0.07  0.28  1  6.7e-16 1.3
Fracture -0.88 -0.25  0.30 -0.22  0.15  1  1.3e-15 1.6
Hardness  0.27  0.92  0.27 -0.10 -0.08  1  1.0e-15 1.4
                       PC1  PC2  PC3  PC4  PC5
SS loadings           3.03 1.30 0.31 0.24 0.12
Proportion Var        0.61 0.26 0.06 0.05 0.02
Cumulative Var        0.61 0.87 0.93 0.98 1.00
Proportion Explained  0.61 0.26 0.06 0.05 0.02
Cumulative Proportion 0.61 0.87 0.93 0.98 1.00
Mean item complexity =  1.7
Fit based upon off diagonal values = 1> 


