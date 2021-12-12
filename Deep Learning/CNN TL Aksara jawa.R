w = 96
h = 96

path = "D:/Datasets/CNN/Aksara Jawa/train"
data = image_from_directory(path, w = w, h = h, grayscale = F)
x_train = data[[1]] 
y_train = data[[2]] 
y_train = factor(y_train, labels = 0:19)

path2 = "D:/Datasets/CNN/Aksara Jawa/test"
data2 = image_from_directory(path2, w = w, h = h, grayscale = F)
x_test = data2[[1]] 
y_test = data2[[2]] 
y_test = factor(y_test, labels = 0:19)


dim(x_train)
dim(x_test)


# ubah dimensi label
y_train = to_categorical(y_train)
y_test = to_categorical(y_test)


dim(y_train)
dim(y_test)


base_model <- application_mobilenet_v2(weights = 'imagenet',
                                    include_top = FALSE,
                                    input_shape = c(96, 96, 3))

predictions <- base_model$output %>% 
  layer_global_average_pooling_2d(trainable = T) %>% 
  layer_dense(32, activation = "relu", trainable = T) %>%
  layer_dropout(0.5, trainable = T) %>%
  layer_dense(20, activation = "softmax", trainable = T)   

model <- keras_model(inputs = base_model$input, outputs = predictions)


for (layer in base_model$layers) layer$trainable <- FALSE

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.005, decay = 1e-6),  ## play with the learning rate
  metrics = "accuracy"
)


history <- model%>%
  fit(x_train,
      y_train,
      batch_size = 32,
      epoch = 10
  )

history
model %>% evaluate(x_test, y_test) 
