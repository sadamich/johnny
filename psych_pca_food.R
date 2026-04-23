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

library("psych")
library("REdaS")
food_m<- food[, 2:6]
bart_spher(food_m)
   Bartlett's Test of Sphericity

Call: bart_spher(x = food_m)

     X2 = 154.994
     df = 10
p-value < 2.22e-16

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



