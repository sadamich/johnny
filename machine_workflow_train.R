https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Workflow/index.html

### Train test split  
library(caret)

# split features and target variable
X <- data[, !(names(data) %in% "temp")] # drop the 'temp' column for features
y <- data$temp # target column

# split the data into training and test sets
set.seed(2) # to make the random split reproducible
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE) # 80% for training
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# print lengths
print(nrow(X_train))


print(nrow(X_test))

print(length(y_train))

print(length(y_test))