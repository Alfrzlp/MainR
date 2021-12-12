library(keras)
library(tidyverse)

reuters <- dataset_reuters(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% reuters

str(train_data)
str(train_labels)


vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}

x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)

x_train %>% dim()
train_data[[1]]
x_train[1,]

length(train_data)

length_data = sapply(train_data, length)
length_data

tibble(length_data = length_data) %>% 
  ggplot(aes(length_data))+
  geom_histogram(binwidth = 20)+
  theme_minimal()+
  ggtitle("Training data length distribution")

max_len = 500

## pad sequence
train <- pad_sequences(train_data, maxlen = max_len, padding = "post")
dim(train)

test <- pad_sequences(test_data, maxlen = max_len, padding = "post")
dim(test)

y_train = train_labels %>% to_categorical()
y_test = test_labels %>% to_categorical()


model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = 10000, output_dim = 50) %>% 
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", strides = 1) %>% 
  layer_global_max_pooling_1d() %>% 
  layer_dropout(0.5) %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dropout(0.5) %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dropout(0.5) %>% 
  layer_dense(units = 46, activation = "softmax")

summary(model)

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  train,
  y_train,
  epochs = 30,
  batch_size = 50
)

history
model %>% evaluate(test, y_test)
