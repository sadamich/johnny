https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Workflow/index.html

### 4 Feature scaling
### 4 1 Min-Max Scaling

# columns to transform
transform_cols <- c("vapor_pres", "pres", "rel_humid")

for (i in transform_cols) {
  # Scale the features for the test set
  X_test[i] <- (X_test[i] - min(X_train[i]) + 1) / (max(X_train[i]) - min(X_train[i]) + 2)

  # Scale the features for the training set
  X_train[i] <- (X_train[i] - min(X_train[i]) + 1) / (max(X_train[i]) - min(X_train[i]) + 2)

  # Apply custom min-max scaling to y_test and y_train
  y_test <- (y_test - min(y_train) + 1) / (max(y_train) - min(y_train) + 2)
  y_train <- (y_train - min(y_train) + 1) / (max(y_train) - min(y_train) + 2)
}

### 4 2 Logistic Transformation

# Print rows where any of the columns in transform_cols have values <= 0
print(X_test[rowSums(X_test[transform_cols] <= 0) > 0, ])

print(y_test[y_test <= 0])

# Print rows in where any of the columns in transform_cols have values >= 1
print(X_test[rowSums(X_test[transform_cols] >= 1) > 0, ])


print(y_test[y_test >= 1])

# Logistic transformation
X_test[transform_cols] <- log(X_test[transform_cols] / (1 - X_test[transform_cols]))
X_train[transform_cols] <- log(X_train[transform_cols] / (1 - X_train[transform_cols]))
y_test <- log(y_test / (1 - y_test))
y_train <- log(y_train / (1 - y_train))


### Inspect new distributions

par(mar = c(4, 4, 0.5, 4), mfrow = c(2, 2), pin = c(2.5, 1.5))
hist(X_train$vapor_pres, main = "vapor_pres (Train)", col = "skyblue", border = "white", breaks = 35)
hist(X_train$pres, main = "pres (Train)", col = "skyblue", border = "white", breaks = 35)
hist(X_train$rel_humid, main = "rel_humid (Train)", col = "skyblue", border = "white", breaks = 35)
hist(y_train, main = "Temperature (Train)", col = "skyblue", border = "white", breaks = 35)

mtext("Distributions of transformed training data, now in real space", side = 3, outer = TRUE, line = - 1.2, cex = 1.4)


par(mar = c(4, 4, 0.5, 4), mfrow = c(2, 2), pin = c(2.5, 1.5))
hist(X_test$vapor_pres, main = "vapor_pres (Test)", col = "salmon", border = "white", breaks = 35)
hist(X_test$pres, main = "pres (Test)", col = "salmon", border = "white", breaks = 35)
hist(X_test$rel_humid, main = "rel_humid (Test)", col = "salmon", border = "white", breaks = 35)
hist(y_test, main = "Temperature (Test)", col = "salmon", border = "white", breaks = 35)

mtext("Distributions of transformed test data, now in real space", side = 3, outer = TRUE, line = -1.2, cex = 1.4)