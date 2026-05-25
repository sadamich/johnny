https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Workflow/index.html

### 5 Model definiton


library(keras)

# Define the model
model <- keras_model_sequential() %>%
  layer_dense(units = 9, activation = "relu", input_shape = dim(X_train)[2]) %>%
  layer_dense(units = 9, activation = "relu") %>%
  layer_dense(units = 1) # Regression output (no activation)


summary(model)


# Compile the model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0001),
  loss = "mean_squared_error"
)