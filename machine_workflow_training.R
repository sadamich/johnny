https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Workflow/index.html

### 6 Model training

# Train the model
history <- model %>% fit(
  as.matrix(X_train), as.matrix(y_train),
  epochs = 100, batch_size = 200,
  validation_split = 0, # No validation data since we already split the data
)