library(keras)
library(doSNOW)
library(tidyverse)

w = 64
h = 64

path = "D:/Datasets/CNN/Pneumonia vs Normal/train"
data = image_from_directory(path, w = w, h = h, grayscale = T)
x_train = data[[1]] 
y_train = data[[2]] 
y_train = factor(y_train, labels = 0:1)

path2 ="D:/Datasets/CNN/Pneumonia vs Normal/test"
data2 = image_from_directory(path2, w = w, h = h, grayscale = T)
x_test = data2[[1]] 
y_test = data2[[2]] 
y_test = factor(y_test, labels = 0:1) 


# ubah dimensi label
y_train = to_categorical(y_train)
y_test = to_categorical(y_test)

dim(x_train)
dim(x_test)

dim(y_train)
dim(y_test)


model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = 3,
                padding = "valid", input_shape = c(w, h, 1), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = 2) %>% 
  layer_dropout(0.2) %>% 
  
  layer_conv_2d(filters = 64, kernel_size = 3,
                padding = "valid", activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(0.2) %>% 
  
  layer_conv_2d(filters = 128, kernel_size = 3,
                padding = "valid", activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(0.2) %>% 
  
  layer_flatten() %>%
  layer_dense(256, activation = "relu") %>%
  layer_dropout(0.5) %>% 
  
  layer_dense(1, activation = "sigmoid")

summary(model)


# compile
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(),
  metrics = "accuracy"
)


# fit
history <- model %>%
  fit(x_train,
      y_train,
      batch_size = 100,
      #class_weight = list(0.7, 0.2),
      epoch = 10
  )
plot(history)

history
# evaluate
model %>% evaluate(x_test, y_test) 
