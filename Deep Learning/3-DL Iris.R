library(keras)
library(tidyverse)

iris <- datasets::iris
iris
iris <- iris %>%
  mutate_if(is.numeric, scale) %>% # ubah skala data
  mutate(Species = as.numeric(factor(iris$Species)) - 1)
# 0 setos     1 versicolor    2 virginica

iris
indeks <- sample(1:nrow(iris), 120)

train <- iris[indeks, ]
train
x_train <- train[, 1:4] %>% as.matrix()
y_train <- train[, 5]


test <- iris[-indeks, ]
test
x_test <- test[, 1:4] %>% as.matrix()
y_test <- test[, 5]

# ubah label
y_train <- to_categorical(y_train)
y_train
y_test <- to_categorical(y_test)
y_test

model <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(4)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax") # sigmoid biasanya 0 1

summary(model)


model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 250, batch_size = 20, # data dimasukkan 20, 20 dst... sampai 120
  validation_split = 0
)

history
plot(history)

# evaluasi
model %>% evaluate(x_test, y_test)

# predict some data
model %>% predict_classes(x_test[1:5, ])
test[, 5][1:5]


# tabel
hasil <- model %>% predict_classes(x_test)
hasil
actual <- test[, 5]
actual
table(Predicted = hasil, Actual = actual)



# predict 1
model %>% predict_classes(t(matrix(c(-1, 1.4, 0.5, 0))))



# save model and load
save_model_hdf5(model, "D:/Datasets/Model/model_iris.h5")
model <- load_model_hdf5("D:/Datasets/Model/model_iris.h5")

model_to_json(model)
