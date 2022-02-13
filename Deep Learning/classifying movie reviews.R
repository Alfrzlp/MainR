a <- 1
b <- 2
c(a, b) %<-% c(b, a)
a

library(keras)

imdb <- dataset_imdb(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb

# list berisi 25.000 1 nya 1 review
str(train_data)
str(train_labels)

train_data[[3]]


class(train_labels)
class(train_labels)

max(sapply(train_data, max))

# word_index is a dictionary mapping words to an integer index
word_index <- dataset_imdb_word_index()
word_index

# We reverse it, mapping integer indices to words
reverse_word_index <- names(word_index)
reverse_word_index
names(reverse_word_index) <- word_index
reverse_word_index

class(reverse_word_index)
reverse_word_index[1:5]

# We decode the review; note that our indices were offset by 3
# because 0, 1 and 2 are reserved indices for "padding", "start of sequence", and "unknown".
decoded_review <- sapply(train_data[[3]], function(index) {
  word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
  if (!is.null(word)) word else "?"
})

decoded_review
cat(decoded_review)


vectorize_sequences <- function(sequences, dimension = 10000) {
  # Create an all-zero matrix of shape (len(sequences), dimension)
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences)) {
    # Sets specific indices of results[i] to 1s
    results[i, sequences[[i]]] <- 1
  }
  results
}

# Our vectorized training data
x_train <- vectorize_sequences(train_data)
# Our vectorized test data
x_test <- vectorize_sequences(test_data)

dim(x_train)
dim(x_test)

x_train[2, ]

str(x_train[1, ])

# Our vectorized labels
y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

# where 0 stands for "negative" and 1 stands for "positive"
y_train
y_test

# sigmoid untuk output 1 dan 0
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
# Ini bukan satu-satunya pilihan yang layak:
# Anda dapat menggunakan, misalnya, `mean_squared_error`
# But crossentropy is usually the best choice when you are
# dealing with models that output probabilities

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),
  loss = loss_binary_crossentropy,
  metrics = metric_binary_accuracy
)

val_indices <- 1:10000
memory.limit(size = 500000)

x_val <- x_train[val_indices, ]
partial_x_train <- x_train[-val_indices, ]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]


# ======================================================================
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)
# =====================================================================
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(x_train, y_train, epochs = 20, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)
results

model %>% predict(x_test[1:10, ])

# The `rmsprop` optimizer is generally a good enough choice,
# whatever your problem. That's one less thing for you to worry about.
