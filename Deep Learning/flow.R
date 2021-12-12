library(keras)
w = 64
h = 64

train_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
  "D:/Datasets/CNN/Montor mobil/Train",
  # This is the data generator
  datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "binary"
)

test_generator <- flow_images_from_directory(
  "D:/Datasets/CNN/Montor mobil/Test",
  image_data_generator(rescale = 1/255),
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "binary"
)

batch <- generator_next(train_generator)
str(batch)
batch[[1]] %>% dim()
batch[[2]] %>% length()

par(mfrow = c(5, 4), pty = "s", mar = c(1, 0, 1, 0))
for (i in 1:20) {
  plot(as.raster(batch[[1]][i,,,]))
}



model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_flatten() %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

# compile
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-4),
  metrics = c("acc")
)


history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 1,
  epochs = 30
)

history

model %>% evaluate_generator(test_generator, steps = 1)








## Using data augmentation
datagen <- image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)

# We pick one image to "augment"
fnames <- list.files("D:/Datasets/CNN/Montor mobil/Train/Mobil", full.names = TRUE)
img_path <- fnames[2]

# Convert it to an array with shape (150, 150, 3)
img <- image_load(img_path, target_size = c(150, 150))
img_array <- image_to_array(img)
img_array <- array_reshape(img_array, c(1, 150, 150, 3))

# Generated that will flow augmented images
augmentation_generator <- flow_images_from_data(
  img_array, 
  generator = datagen, 
  batch_size = 1 
)

# Plot the first 4 augmented images
par(mfrow = c(5, 4), pty = "s", mar = c(1, 0, 1, 0))
for (i in 1:6) {
  batch <- generator_next(augmentation_generator)
  plot(as.raster(batch[1,,,]))
}
par(op)



# build model -----------------------------------------------
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_flatten() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")  

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-4),
  metrics = c("acc")
)

summary(model)

# image preporcessing--------------------------------------------------
datagen <- image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE
)

test_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
  "D:/Datasets/CNN/Montor mobil/Train",
  datagen,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "binary"
)

test_generator <- flow_images_from_directory(
  "D:/Datasets/CNN/Montor mobil/Test",
  test_datagen,
  target_size = c(150, 150),
  batch_size = 10,
  class_mode = "binary"
)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 200/32, # n / batch size
  epochs = 50
)

model %>% evaluate_generator(test_generator, steps = 2)
