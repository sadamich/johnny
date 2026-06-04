https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Artifical-Neural-Networks/Multilayer-Perceptron-Algorithm/index.html


### Multilayer Perceptron Algorithm

### Plot Function ###

library(ggplot2)

plot_decision_boundary <- function(X, y, model) {
  # Predicted probabilities from the model
  prob_grid <- expand.grid(
    X1 = seq(min(X[, 1]) - 1, max(X[, 1]) + 1, by = 0.02),
    X2 = seq(min(X[, 2]) - 1, max(X[, 2]) + 1, by = 0.02)
  )
  prob_grid$pred <- predict(model, newdata = prob_grid, type = "response")

  # Plot
  ggplot() +
    geom_tile(data = prob_grid, aes(x = X1, y = X2, fill = pred), alpha = 0.3) +
    geom_contour(data = prob_grid, aes(x = X1, y = X2, z = pred), breaks = 0.5, color = "black") +
    geom_point(data = data.frame(X, y), aes(x = X1, y = X2, color = factor(y), shape = factor(y)), size = 3) +
    scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
    scale_color_manual(values = c("dodgerblue", "firebrick1")) +
    scale_shape_manual(values = c("0" = 15, "1" = 19)) +
    labs(title = "Multi-Layer Perceptron", x = "Feature 1", y = "Feature 2", fill = "Prediction", color = "Class", shape = "Class") +
    theme_minimal() +
    theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.5, size = 18))
}


### Toy Data ###

# Set seed for reproducibility
set.seed(0)

# Create some toy data
X <- matrix(rnorm(60), ncol = 2)
colnames(X) <- c("X1", "X2")

# Divide the data into two classes along the line x2 = -x1
y <- ifelse(X[, 1] + X[, 2] >= 0, 1, 0)


library(neuralnet)

mlp <- neuralnet(y ~ X1 + X2,
  data = data.frame(X, y = y), hidden = 4,
  algorithm = "rprop+", linear.output = FALSE,
  stepmax = 500, rep = 1
)

plot_decision_boundary(X, y, mlp)



### XOR Problem 

et.seed(0)

# Create some toy data
X <- matrix(rnorm(30 * 2), ncol = 2)

# Divide the data
y <- ifelse(X[, 1] * X[, 2] >= 0, 1, 0)


logreg <- glm(y ~ X1 + X2, family = binomial(), data = data.frame(X, y))

plot_decision_boundary(X, y, logreg) + labs(title = "Logistic Regression")


mlp <- neuralnet(y ~ X1 + X2,
  data = data.frame(X, y = y), hidden = 4,
  algorithm = "rprop+", linear.output = FALSE,
  stepmax = 500, rep = 1
)

plot_decision_boundary(X, y, mlp)

