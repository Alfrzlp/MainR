library(keras)
library(tidyverse)

c(c(x_train, y_train), c(x_test, y_test)) %<-% dataset_boston_housing()

x_train
y_train

# 404 training samples and 102 test samples

# 1. Per capita crime rate.
# 2. Proportion of residential land zoned for lots over 25,000 square feet.
# 3. Proportion of non-retail business acres per town.
# 4. Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# 5. Nitric oxides concentration (parts per 10 million).
# 6. Average number of rooms per dwelling.
# 7. Proportion of owner-occupied units built prior to 1940.
# 8. Weighted distances to five Boston employment centres.
# 9. Index of accessibility to radial highways.
# 10. Full-value property-tax rate per $10,000.
# 11. Pupil-teacher ratio by town.
# 12. 1000 * (Bk - 0.63) ** 2 where Bk is the proportion of Black people by town.
# 13. % lower status of the population.
#
# The targets are the median values of owner-occupied homes,
# in thousands of dollars:

mean <- apply(x_train, 2, mean)
std <- apply(x_train, 2, sd)

x_train <- scale(x_train, center = mean, scale = std)
x_test <- scale(x_test, center = mean, scale = std)


build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(
      units = 64, activation = "relu",
      input_shape = dim(x_train)[[2]]
    ) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) # tanpa aktivasi maka akan liniear

  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae") # Mean Absolute Error
  )
}



k <- 4
indices <- sample(1:nrow(x_train))
folds <- cut(1:length(indices), breaks = k, labels = FALSE)

num_epochs <- 100
all_scores <- c()
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- x_train[val_indices, ]
  val_targets <- y_train[val_indices]

  # Prepare the training data: data from all other partitions
  partial_train_data <- x_train[-val_indices, ]
  partial_train_targets <- y_train[-val_indices]

  # Build the Keras model (already compiled)
  model <- build_model()

  # Train the model (in silent mode, verbose=0)
  model %>% fit(partial_train_data, partial_train_targets,
    epochs = num_epochs, batch_size = 1, verbose = 1
  )

  # Evaluate the model on the validation data
  results <- model %>% evaluate(val_data, val_targets, verbose = 1)
  all_scores <- c(all_scores, results[[2]])
}


all_scores
mean(all_scores)

k_clear_session()




# karena mae masih terbilang cukup besar, maka kita coba epoch 500
num_epochs <- 500
all_mae_histories <- NULL
for (i in 1:k) {
  cat("processing fold #", i, "\n")

  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices, ]
  val_targets <- train_targets[val_indices]

  # Prepare the training data: data from all other partitions
  partial_train_data <- train_data[-val_indices, ]
  partial_train_targets <- train_targets[-val_indices]

  # Build the Keras model (already compiled)
  model <- build_model()

  # Train the model (in silent mode, verbose=0)
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 1
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

average_mae_history <- data.frame(
  epoch = seq(1:ncol(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)

ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) +
  geom_line()

ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) +
  geom_smooth()



# Get a fresh, compiled model.
model <- build_model()

# Train it on the entirety of the data.
model %>% fit(x_train, y_train,
  epochs = 80, batch_size = 16, verbose = 1
)

result <- model %>% evaluate(test_data, test_targets)
result

# * Regression is done using different loss functions from classification;
#   Mean Squared Error (MSE) is a commonly used loss function for regression.
# * Similarly, evaluation metrics to be used for regression differ
#   from those used for classification; naturally the concept of "accuracy"
#   does not apply for regression. A common regression metric is Mean Absolute Error (MAE).
# * When features in the input data have values in different ranges,
#   each feature should be scaled independently as a preprocessing step.
# * When there is little data available, using K-Fold validation
#   is a great way to reliably evaluate a model.
# * When little training data is available, it is preferable to use
#   a small network with very few hidden layers (typically only one or two),
#   in order to avoid severe overfitting.
