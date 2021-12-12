library(keras)

# create the base pre-trained model
base_model <- application_inception_v3(weights = 'imagenet', include_top = FALSE)

# add our custom layers
predictions <- base_model$output %>% 
  layer_global_average_pooling_2d() %>% 
  layer_dense(units = 1024, activation = 'relu') %>% 
  layer_dense(units = 200, activation = 'softmax')

# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)

# first: train only the top layers (which were randomly initialized)
# i.e. freeze all convolutional InceptionV3 layers
for (layer in base_model$layers)
  layer$trainable <- FALSE

# compile the model (should be done *after* setting layers to non-trainable)
model %>% compile(optimizer = 'rmsprop', loss = 'categorical_crossentropy')

# train the model on the new data for a few epochs
model %>% fit_generator(...)

# at this point, the top layers are well trained and we can start fine-tuning
# convolutional layers from inception V3. We will freeze the bottom N layers
# and train the remaining top layers.

# let's visualize layer names and layer indices to see how many layers
# we should freeze:
layers <- base_model$layers
for (i in 1:length(layers))
  cat(i, layers[[i]]$name, "\n")

# we chose to train the top 2 inception blocks, i.e. we will freeze
# the first 172 layers and unfreeze the rest:
for (i in 1:172)
  layers[[i]]$trainable <- FALSE
for (i in 173:length(layers))
  layers[[i]]$trainable <- TRUE

# we need to recompile the model for these modifications to take effect
# we use SGD with a low learning rate
model %>% compile(
  optimizer = optimizer_sgd(lr = 0.0001, momentum = 0.9), 
  loss = 'categorical_crossentropy'
)

# we train our model again (this time fine-tuning the top 2 inception blocks
# alongside the top Dense layers
model %>% fit_generator(...)

keras::application_vgg16()
