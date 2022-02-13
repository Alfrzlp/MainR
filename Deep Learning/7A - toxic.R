## Multi-label classification using data from Kaggle Competition:
## id 8076
## url https://www.kaggle.com/c/jigsaw-toxic-comment-classification-challenge

## load libraries
library(here)
library(dplyr)
library(readr)
library(keras)

## define parameters
vocab_size <- 25000
max_len <- 250
embedding_dims <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 256

## TODO ## ---------------------------------------------------------------------
## Add part to download data and put in /data/toxic_comments/

## load data -------------------------------------------------------------------
train_data <- read_csv(paste0(here::here(), "/data/toxic_comments/train.csv"))
test_data <- read_csv(paste0(here::here(), "/data/toxic_comments/test.csv"))

## use keras tokenizer
tokenizer <- text_tokenizer(num_words = vocab_size) %>%
  fit_text_tokenizer(train_data$comment_text)

## create sequances
train_seq <- texts_to_sequences(tokenizer, train_data$comment_text)
test_seq <- texts_to_sequences(tokenizer, test_data$comment_text)

## pad sequence
x_train <- pad_sequences(train_seq, maxlen = max_len, padding = "post")
x_test <- pad_sequences(test_seq, maxlen = max_len, padding = "post")

## extract targets columns and convert to matrix
y_train <- train_data %>%
  select(toxic:identity_hate) %>%
  as.matrix()


## define model
model <- keras_model_sequential() %>%
  # embedding layer
  layer_embedding(
    input_dim = vocab_size,
    output_dim = embedding_dims,
    input_length = max_len
  ) %>%
  layer_dropout(0.2) %>%
  # add a Convolution1D
  layer_conv_1d(
    filters, kernel_size,
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  # apply max pooling
  layer_global_max_pooling_1d() %>%
  # add fully connected layer:
  layer_dense(hidden_dims) %>%
  # apply 20% layer dropout
  layer_dropout(0.2) %>%
  layer_activation("relu") %>%
  # output layer then sigmoid
  layer_dense(6) %>%
  layer_activation("sigmoid")


## specify model optimizer, loss and metrics
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)


## fit
history2 <- model %>%
  fit(x_train, y_train,
    epochs = 4,
    batch_size = 64,
    validation_split = 0.05,
    verbose = 1
  )


## predict on test data --------------------------------------------------------
predicted_prob <- predict_proba(model, x_test)

## join ids and predictions -------------------------------------------
res <- as_data_frame(predicted_prob)
names(res) <- names(train_data)[3:8] ## labels names
res <- tibble::add_column(res, id = test_data$id, .before = 1)









# CNN + LSTM ----------------------------------------------------
## define hyper parameters
emd_size <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 256
pool_size <- 4
lstm_op_size <- 64

model_cnn_lstm <- keras_model_sequential() %>%
  # embedding layer
  layer_embedding(
    input_dim = vocab_size,
    output_dim = emd_size,
    input_length = max_len
  ) %>%
  layer_dropout(0.2) %>%
  # add a Convolution1D
  layer_conv_1d(
    filters, kernel_size,
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  ## add pooling layer
  layer_max_pooling_1d(pool_size = pool_size) %>%
  ## add lstm layer
  layer_lstm(units = lstm_op_size) %>%
  # add fully connected layer:
  layer_dense(hidden_dims) %>%
  # apply 20% layer dropout
  layer_dropout(0.2) %>%
  layer_activation("relu") %>%
  # output layer then sigmoid
  layer_dense(6) %>%
  layer_activation("sigmoid")

## display model summary
summary(model_cnn_lstm)





# GRU + CNN--------------------------------------------------------
## define hyper parameters
emd_size <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 256
gru_units <- 128

## define model
model_gru_cnn <- keras_model_sequential() %>%
  # embedding layer
  layer_embedding(
    input_dim = vocab_size,
    output_dim = emd_size,
    input_length = max_len
  ) %>%
  layer_dropout(0.2) %>%
  ## add bidirectional gru
  bidirectional(layer_gru(units = gru_units, return_sequences = TRUE)) %>%
  ## add a Convolution1D
  layer_conv_1d(
    filters, kernel_size,
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  ## add pooling layer
  layer_global_max_pooling_1d() %>%
  # add fully connected layer:
  layer_dense(hidden_dims) %>%
  # apply 20% layer dropout
  layer_dropout(0.2) %>%
  layer_activation("relu") %>%
  # output layer then sigmoid
  layer_dense(6) %>%
  layer_activation("sigmoid")

## display model summary
summary(model_gru_cnn)
