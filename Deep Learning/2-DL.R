library(keras)

x_train = seq(-20, 20, by=0.5)
x_train
y_train = sqrt(2*x_train^2 + 1)
y_train

x_test = seq(1, 20, by=0.5)
x_test
y_test = sqrt(2*x_test^2 + 1)
y_test

model <- keras_model_sequential() %>% 
  layer_dense(units = 8, activation = "relu", input_shape = c(1)) %>% 
  layer_dense(units = 4, activation = "relu") %>% 
  layer_dense(units = 1, activation = "linear")


model %>% compile(
  optimizer = optimizer_sgd(lr = 0.001),
  loss = "mse"
)

model %>% fit(
  x_train, y_train,
  epochs = 100, batch_size = 30
)

model %>% evaluate(
  x_test, y_test
)

model %>% predict(30)
