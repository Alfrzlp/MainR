library(keras)
library(EBImage)

train <- flow_images_from_directory("D:\\Datasets\\CNN\\Montor mobil\\Train",
  target_size = c(20, 20)
)
test <- flow_images_from_directory("D:\\Datasets\\CNN\\Montor mobil\\Test",
  target_size = c(20, 20)
)

train$class_indices
train$filepaths

img <- readImage(train$filepaths[1])
display(img)


model <- keras_model_sequential() %>%
  layer_conv_2d(
    filters = 32, kernel_size = 3,
    padding = "valid", input_shape = c(32, 32, 3),
    activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 16, kernel_size = 3) %>%
  layer_activation_leaky_relu(0.2) %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(0.25) %>%
  layer_flatten() %>%
  layer_dense(64, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(2, activation = "softmax")


summary(model)
model <- application_vgg16()


model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = "accuracy"
)



history <- model %>%
  fit(train,
    trainLabels,
    epoch = 30
  )

history
model %>% evaluate(test, testLabels)

# Tabel
pred <- model %>% predict_classes(test)
table(Predicted = pred, Actual = testy)

# save_model_hdf5(model , "D:/Datasets/Model/model_mm.h5")
model <- load_model_hdf5("D:/Datasets/Model/model_mm.hdf5")
