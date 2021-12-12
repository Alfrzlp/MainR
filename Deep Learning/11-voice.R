library(keras)
library(tidyverse)

df = read.csv("D:/Datasets/voice.csv")
dim(df)

sample_n(df, 5)

data = df %>% select(-label) %>% as.matrix()
label = df %>% mutate(label = if_else(label == "male" , 1, 0)) %>% 
    select(label) %>% pull() %>% to_categorical()

dim(data)
dim(label)

set.seed(3)
indeks = sample(1:3168, 3168*0.8)
indeks %>% length()

x_train = data[indeks,]
y_train = label[indeks,]

x_test = data[-indeks,]
y_test = label[-indeks,]


model <- keras_model_sequential() %>% 
  layer_dense(128, activation = "relu", input_shape = c(20)) %>% 
  layer_dropout(0.2) %>% 
  layer_dense(32, activation = "relu") %>% 
  layer_dense(2, activation = "sigmoid")

summary(model)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)


history <- model %>% fit(
  x_train, y_train, 
  epochs = 120, 
  batch_size = 32,
  validation_split = 0.2
)
history

model %>% evaluate(x_test, y_test)
model2 %>% evaluate(x_test, y_test)
plot(history)







