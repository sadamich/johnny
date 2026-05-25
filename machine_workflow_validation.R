https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Workflow/index.html

### Model evaluation

###1 Convergence of training

plot(history)

history

### 2 Model validation

eval <- model %>% evaluate(as.matrix(X_test), as.matrix(y_test))


### k-Fold Cross-Validation

## 1. define a function for creating the model
create_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 9, activation = "relu", input_shape = dim(X_train)[2]) %>%
    layer_dense(units = 9, activation = "relu") %>%
    layer_dense(units = 1) # assuming a regression

  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.0001),
    loss = "mean_squared_error"
  )
  return(model)
}

## 2. create k folds
set.seed(123) # For reproducibility
folds <- createFolds(y_train, k = 10, list = TRUE)
results <- vector("list", length(folds))


## 3. loop over all folds
for (i in seq_along(folds)) {
  # Split the data
  train_indices <- folds[[i]]
  X_train_fold <- X_train[train_indices, ]
  y_train_fold <- y_train[train_indices]

  test_indices <- setdiff(seq_len(nrow(X_train)), train_indices)
  X_test_fold <- X_train[test_indices, ]
  y_test_fold <- y_train[test_indices]

  # Create and compile the model
  model <- create_model()

  # Fit the model
  history <- model %>% fit(
    as.matrix(X_train_fold), as.matrix(y_train_fold),
    epochs = 100, batch_size = 200
  )

  # Evaluate the model
  results[[i]] <- model %>% evaluate(as.matrix(X_test_fold), as.matrix(y_test_fold))
}
# Calculate average performance
average_loss <- mean(unlist(results))
print(average_loss)

### 3 Learning curve

library(ggplot2)

set.seed(123)

# Define a sequence of training sizes
train_sizes <- seq(0.1, 1, by = 0.1)

# Store errors
train_error <- numeric(length(train_sizes))
validation_error <- numeric(length(train_sizes))

i <- 1
for (size in train_sizes) {
  # Sample subset of training data
  index <- sample(1:nrow(X_train), size * nrow(X_train))
  subset_data <- X_train[index, ]
  subset_target <- y_train[index]

  # Train model
  model <- create_model()
  history <- model %>% fit(as.matrix(subset_data), subset_target, epochs = 100, batch_size = 200, verbose = 0)

  # Predict and compute error on training subset
  train_error[i] <- min(unlist(history$metrics))
  validation_error[i] <- model %>% evaluate(as.matrix(X_test), as.matrix(y_test))

  i <- i + 1
}

# Create data frame for plotting
plot_data <- data.frame(
  TrainSize = rep(train_sizes, 2),
  error = c(train_error, validation_error),
  DataType = factor(rep(c("Train", "Validation"), each = length(train_sizes)))
)

# Plot learning curve
ggplot(plot_data, aes(x = TrainSize * nrow(X_train), y = error, color = DataType)) +
  geom_line() +
  labs(
    title = "Learning Curve",
    x = "Training Size",
    y = "Error",
    color = "Data Type"
  ) +
  theme_minimal()
