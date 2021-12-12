library(keras)
library(tidyverse)

data = read.csv(paste0(getwd(),"/Main R/dmkm-main/r/dataset/data_ipeh.csv"))

# normalisasi
data = data %>% mutate_at(2:4, ~scale(.x))


# split data
set.seed(666)
sampel <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[sampel==1, ]
test <- data[sampel==2, ]


x_train = train %>% select(-1) %>% as.matrix()
y_train = train$admit %>% to_categorical()

x_test = test %>% select(-1) %>% as.matrix()
y_test = test$admit %>% to_categorical()

dim(x_train)
dim(x_test)


model <- keras_model_sequential() %>% 
  layer_dense(64, activation = "relu", input_shape = c(3)) %>% 
  layer_dense(32, activation = "relu") %>% 
  layer_dense(16, activation = "relu") %>% 
  layer_dense(2, activation = "sigmoid")

summary(model)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 100, 
  batch_size = 50,
  validation_split = 0.2
)

history
model %>% evaluate(x_test, y_test)
