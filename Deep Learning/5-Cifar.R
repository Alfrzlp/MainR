library(keras)

cifar = dataset_cifar10()

c(x_train, y_train) %<-% cifar$train
c(x_test, y_test) %<-% cifar$test


class_names <- c('airplane', 'automobile', 'bird', 'cat', 'deer',
                 'dog', 'frog', 'horse', 'ship', 'truck')

# cek dimensi
dim(x_train)
dim(x_test)

dim(y_train)
dim(y_test)

# ubah dimensi 
y_train = to_categorical(y_train)
y_test = to_categorical(y_test)

dim(y_train)
dim(y_test)


# membuat model
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 16, kernel_size = 3, input_shape = c(32, 32, 3),
                activation = "relu", padding = "same") %>% 
  layer_max_pooling_2d(2) %>% 

  layer_conv_2d(filters = 32, kernel_size = 3,
              activation = "relu", padding = "same") %>% 
  layer_max_pooling_2d(2) %>% 
  
  layer_conv_2d(filters = 64, kernel_size = 3,
                activation = "relu", padding = "valid") %>% 
  layer_max_pooling_2d(2) %>% 
    
  layer_flatten() %>% 
  layer_dense(32, activation = "relu") %>% 
    
  layer_dense(10, activation = "softmax")



model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)


history <- model %>% 
  fit(x_train, y_train,
  batch_size = 30,
  epoch = 50
  )


model %>% evaluate(x_test, cifar$test$y)


