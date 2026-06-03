https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Artifical-Neural-Networks/index.html

### Artifical neural networks (ANN)                                        ###
library(ggplot2)
library(reshape2)
### he perceptron algorithm                                                ###

plot_decision_boundary <- function(X, y, classifier, resolution = 0.02) {
  # Define the grid
  x1_min <- min(X[, 1]) - 1
  x1_max <- max(X[, 1]) + 1
  x2_min <- min(X[, 2]) - 1
  x2_max <- max(X[, 2]) + 1
  xx1 <- seq(x1_min, x1_max, by = resolution)
  xx2 <- seq(x2_min, x2_max, by = resolution)
  grid <- expand.grid(xx1 = xx1, xx2 = xx2)

  p <- ggplot()


if (!is.null(classifier)) {
    # For each grid point, predict the class
    lab <- classifier$predict(as.matrix(grid), classifier$weights, classifier$bias)
    grid$lab <- lab

    # Plot the decision regions
    p <- p + geom_tile(data = grid, aes(x = xx1, y = xx2, fill = factor(lab)), alpha = 0.3) +
      scale_fill_manual(values = c("blue", "orange"))
  }

 # Plot the data points
  data <- data.frame(X, Class = as.factor(y))
  p <- p + geom_point(data = data, aes(x = X[, 1], y = X[, 2], color = Class, shape = Class), alpha = 0.8, size = 3) +
    scale_color_manual(values = c("blue", "orange")) +
    scale_shape_manual(values = c(19, 15)) +
    labs(x = "Feature 1", y = "Feature 2") +
    theme_minimal()

  return(p)
}

### implemention of a simple version of a perceptron.                      ###

SimplePerceptron <- function(epochs, eta = 1) {
  # Define the list to hold the perceptron parameters
  perceptron <- list(epochs = epochs, eta = eta)
  # Initialize weights and bias
  perceptron$weights <- rnorm(ncol(X))
  perceptron$bias <- runif(1) * 2 - 0.8


  # Define the fit function
  perceptron$fit <- function(X, y) {
    steps <- 0
    # Iterate over epochs
    for (e in 1:perceptron$epochs) {
      for (i in 1:nrow(X)) {
        xi <- X[i, ]
        yi <- y[i]
        prediction <- perceptron$predict(xi, perceptron$weights, perceptron$bias)
        error <- yi - prediction
        perceptron$weights <- perceptron$weights + perceptron$eta * error * xi
        perceptron$bias <- perceptron$bias + perceptron$eta * error

        if (error != 0) {
          steps <- steps + 1
          plot <- plot_decision_boundary(X, y, perceptron) +
            ggtitle(paste("Update No.", steps)) +
            labs(x = "Feature 1", y = "Feature 2") +
            theme(legend.position = "bottom") +
            guides(fill = FALSE)
          print(plot)
        }
      }
    }
  }

  # Define the net input function
  perceptron$net_input <- function(X, weights, bias) {
    return(as.numeric(X %*% weights + bias))
  }

  # Define the predict function
  perceptron$predict <- function(X, weights, bias) {
    return(ifelse(perceptron$net_input(X, weights, bias) >= 0, 1, 0))
  }

  return(perceptron)
}

###  Create toy data
set.seed(0)
X <- matrix(rnorm(60), ncol = 2)
y <- ifelse(X[, 1] + X[, 2] >= 0, 1, 0)

### Plot the data
plot_decision_boundary(X, y, NULL) +
  ggtitle("Toy data") +
  theme(legend.position = "bottom") +
  guides(fill = FALSE)


ppn <- SimplePerceptron(epochs = 1, eta = 0.1)
ppn$fit(X, y)
