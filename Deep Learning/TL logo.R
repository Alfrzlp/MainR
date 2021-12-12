library(keras)
### Keras transfer learning example

################### Section 1 #########################
img_width <- 32
img_height <- 32
batch_size <- 8

train_directory <- "D:/Datasets/CNN/logo/train"
test_directory <- "D:/Datasets/CNN/logo/test"

train_generator <- flow_images_from_directory(train_directory, generator = image_data_generator(),
                                              target_size = c(img_width, img_height), color_mode = "rgb",
                                              class_mode = "categorical", batch_size = batch_size, shuffle = TRUE,
                                              seed = 123)


validation_generator <- flow_images_from_directory(test_directory, generator = image_data_generator(),
                                                   target_size = c(img_width, img_height), color_mode = "rgb", classes = NULL,
                                                   class_mode = "categorical", batch_size = batch_size, shuffle = TRUE,
                                                   seed = 123)
train_samples = 498
validation_samples = 177

################### Section 2 #########################
#base_model <- application_inception_v3(weights = 'imagenet', include_top = FALSE)
base_model <- application_vgg16(weights = 'imagenet', include_top = FALSE)
### use vgg16 -  as inception won't converge --- 

################### Section 3 #########################
## add your custom layers

predictions <- base_model$output %>% 
  layer_global_average_pooling_2d(trainable = T) %>% 
  layer_dense(64, activation = "relu", trainable = T) %>%
  layer_dropout(0.4, trainable = T) %>%
  layer_dense(27, trainable=T) %>%    ## important to adapt to fit the 27 classes in the dataset!
  layer_activation("softmax", trainable=T)

# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)

################### Section 4 #########################
for (layer in base_model$layers) layer$trainable <- FALSE

################### Section 5 #########################
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.003, decay = 1e-6),  ## play with the learning rate
  metrics = "accuracy"
)

hist <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = as.integer(train_samples/batch_size), 
  epochs = 20, 
  validation_data = validation_generator,
  validation_steps = as.integer(validation_samples/batch_size),
  verbose=1,
)
hist
### saveable data frame obejct.
histDF <- data.frame(acc = unlist(hist$history$acc), val_acc=unlist(hist$history$val_acc), val_loss = unlist(hist$history$val_loss),loss = unlist(hist$history$loss))
histDF
