https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Artifical-Neural-Networks/Widrow-Hoff-rule/index.html

### the logistic regression

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
    geom_point(data = data.frame(X, y), aes(x = X1, y = X2, color = factor(y))) +
    scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
    scale_color_manual(values = c("dodgerblue", "firebrick1")) +
    labs(title = "Logistic Regression", x = "Feature 1", y = "Feature 2", fill = "Prediction", color = "Class") +
    theme_minimal()
}

# Set seed for reproducibility
set.seed(0)

# Create some toy data
X <- matrix(rnorm(60), ncol = 2)
colnames(X) <- c("X1", "X2")

# Divide the data into two classes along the line x2 = -x1
y <- ifelse(X[, 1] + X[, 2] >= 0, 1, 0)


# Fit a logistic regression model
logreg_model <- glm(y ~ X1 + X2, family = binomial(), data = data.frame(X, y))

summary(logreg_model)


plot_decision_boundary(X, y, logreg_model)





