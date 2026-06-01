https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Classification-example/index.html

### Decision tree and random forest

### Decision tree

set.seed(0)
X <- matrix(rnorm(60), ncol = 2)
y <- ifelse(X[, 1] * X[, 2] >= 0, 1, 0)

# Create a data frame from the matrix and labels
data_df <- data.frame(X1 = X[, 1], X2 = X[, 2], y = factor(y))

# Define colors for classes
class_colors <- c("dodgerblue", "firebrick1")

# Plot the data with custom colors
plot(data_df$X1, data_df$X2,
  col = class_colors[data_df$y], pch = 19,
  xlab = "Feature 1", ylab = "Feature 2", main = "Scatter Plot of Data"
)
legend("topright", legend = c("Class 0", "Class 1"), col = class_colors, pch = 19, title = "Classes")


### the tree function

library(ggplot2)
library(tree)

# decision tree model
model <- tree(y ~ ., data = data_df)


### Plot result ###
# grid of values for prediction
df <- expand.grid(
  X1 = seq(min(data_df$X1), max(data_df$X1), length.out = 100),
  X2 = seq(min(data_df$X2), max(data_df$X2), length.out = 100)
)

# predicted outcome for each grid point
df$y_pred <- predict(model, df, type = "class")

# visualize decision boundary
ggplot(data_df, aes(X1, X2, fill = y)) +
  geom_raster(data = df, aes(fill = y_pred), alpha = 0.5) +
  geom_point(shape = 21, size = 3) +
  theme_minimal() +
  ggtitle("Decision Tree Decision Boundary for Binary Classification") +
  labs(fill = "Predicted y") +
  theme(plot.title = element_text(hjust = 0.5))


library(rpart)
library(rpart.plot)

dt <- rpart(y ~ X, method = "class", data = data.frame(y, X))
printcp(dt) # display the results


# Plotting Decision Tree
rpart.plot(dt, extra = 106)


### Random forest

library(randomForest)
library(ggplot2)

# random forest model
rf <- randomForest(y ~ X1 + X2, data = data_df, proximity = TRUE, ntree = 100)

df <- expand.grid(
  X1 = seq(min(data_df$X1), max(data_df$X1), length.out = 100),
  X2 = seq(min(data_df$X2), max(data_df$X2), length.out = 100)
)

# predicted outcome for each grid point
df$y_pred <- predict(rf, df)

#  visualize decision boundary
ggplot(data_df, aes(X1, X2, fill = y)) +
  geom_raster(data = df, aes(fill = y_pred), alpha = 0.5) +
  geom_point(shape = 21, size = 3) +
  theme_minimal() +
  ggtitle("Random Forest Decision Boundary for Binary Classification") +
  labs(fill = "Predicted y") +
  theme(plot.title = element_text(hjust = 0.5))