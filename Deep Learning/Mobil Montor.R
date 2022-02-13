library(keras)
library(doSNOW)
library(tidyverse)

w <- 64
h <- 64
path <- "D:/Datasets/CNN/Montor mobil/Train"
train <- image_from_directory(path, w = w, h = h, grayscale = F)
x_train <- train[[1]]
y_train <- factor(train[[2]], labels = 0:1) %>% to_categorical()


path <- "D:/Datasets/CNN/Montor mobil/Test"
test <- image_from_directory(path, w = w, h = h, grayscale = F)
x_test <- test[[1]]
y_test <- factor(test[[2]], labels = 0:1) %>% to_categorical()

dim(x_train)
dim(y_train)

dim(x_test)
dim(y_test)

# build model
model <- keras_model_sequential() %>%
  layer_conv_2d(32,
    kernel_size = 3, input_shape = c(w, h, 3),
    padding = "valid", activation = "relu"
  ) %>%
  layer_max_pooling_2d(2) %>%
  layer_conv_2d(64, kernel_size = 3, activation = "relu") %>%
  layer_max_pooling_2d(2) %>%
  layer_conv_2d(128, kernel_size = 3, activation = "relu") %>%
  layer_max_pooling_2d(2) %>%
  layer_flatten() %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(2, activation = "softmax", name = "output")

summary(model)

# compile
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)


# fit
history <- model %>%
  fit(
    x_train, y_train,
    batch_size = 30,
    epochs = 25
  )
history

model %>% evaluate(x_test, y_test)


# tabel
hasil <- model %>% predict_classes(x_test)
table(prediksi = hasil, Actual = test[[2]])



model2 <- load_model_hdf5("D:/Datasets/Model/model_mm.h5")
