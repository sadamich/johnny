https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Nearest-Neighbors-Algorithm/index.html

### The k-Nearest Neighbors Algorithm


library(ggplot2)

# Create synthetic data for 3 classes
set.seed(42)
n <- 50 # number of samples per class
data <- data.frame(
  x = c(rnorm(n, 2), rnorm(n, 6), rnorm(n, 4)),
  y = c(rnorm(n, 3), rnorm(n, 7), rnorm(n, 4)),
  class = factor(rep(1:3, each = n))
)

# Define new data points
new_points <- data.frame(
  x = c(1, 5, 4, 2, 3, 1),
  y = c(2, 6, 5, 5, 2, 7)
)

# Plotting data
library(ggplot2)

plot_classification <- function(data, classes) {
  p <- ggplot(data) +
    geom_point(aes(x = x, y = y, color = factor(classes), shape = factor(classes)), size = 3) +
    scale_color_manual(values = c("blue", "darkgreen", "purple"), name = "Class") +
    scale_shape_manual(values = c(15, 17, 18), name = "Class") +
    labs(
      title = "k-NN Classification",
      x = "Feature 1",
      y = "Feature 2"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      axis.title.x = element_text(angle = 0, vjust = 1, size = 14),
      axis.title.y = element_text(angle = 90, vjust = 2, size = 14)
    )

  return(p)
}

plot_classification(data, data$class) +
  geom_point(data = new_points, aes(x = x, y = y), color = "red", size = 3, shape = 16) +
  labs(title = "Toy Data")

.default(1, 1, 1, 1, "cm") : 1 is not a factor????

### Class
library(class)

# Specifying number of neighbors
k <- 7

# Performing k-NN classification
predicted_labels <- knn(
  train = data[, c("x", "y")],
  test = new_points[, c("x", "y")],
  cl = data$class,
  k = k
)
new_classified_points <- new_points
new_classified_points$class <- predicted_labels
extended_data <- rbind(data, new_classified_points)
plot_classification(extended_data, extended_data$class) +
  geom_point(data = new_points, aes(x = x, y = y), color = "red", size = 4, shape = 5)

.default(1, 1, 1, 1, "cm") : 1 is not a factor????



### The k-NN Algorithm

# create training data
set.seed(42)
n <- 25 # number of samples per class
train_data <- data.frame(
  x = c(rnorm(n, 2.5, 0.7), rnorm(n, 5, 0.8), rnorm(n, 4)),
  y = c(rnorm(n, 4, 0.7), rnorm(n, 4.5, 0.8), rnorm(n, 4)),
  class = factor(rep(1:3, each = n))
)

# create test data
set.seed(43)
new_points <- data.frame(x = rnorm(4, mean = 4, sd = 1.5), y = rnorm(4, mean = 4, sd = 1.5))
new_points$class <- c(0, 0, 0, 0)

# Visualize Original Data
plot_classification(train_data, train_data$class) +
  geom_point(data = new_points, aes(x = x, y = y), color = "red", size = 3, shape = 16) +
  labs(title = paste("Training Data and new Points"))


