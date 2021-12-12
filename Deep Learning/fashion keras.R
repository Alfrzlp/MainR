library(keras)

fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

#The following shows there are 60,000 images in the training set,
#with each image represented as 28 x 28 pixels
dim(train_images)
dim(train_labels)
train_labels[1:20]

dim(test_images)
dim(test_labels)

View(train_images) #60000 baris 50 kolom
View(train_labels)

#proses data

#The data must be preprocessed before training the network.
#If you inspect the first image in the training set,
#you will see that the pixel values fall in the range of 0 to 255

library(tidyr)
library(ggplot2)

image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")


#We scale these values to a range of 0 to 1 before feeding to the neural network model.
#For this, we simply divide by 255.

train_images <- train_images / 255
test_images <- test_images / 255


#Display the first 25 images from the training set and display the class name below each image.
#Verify that the data is in the correct format and we're ready to build and train the network
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}

#Build the model
#Building the neural network requires configuring the layers of the model,
#then compiling the model.

#setup the layer
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')
# The first layer in this network, layer_flatten,
# transforms the format of the images from a 2d-array (of 28 by 28 pixels),
# to a 1d-array of 28 * 28 = 784 pixels.
# Think of this layer as unstacking rows of pixels in the image and lining them up.
# This layer has no parameters to learn; it only reformats the data.

# After the pixels are flattened, the network consists of a sequence of two dense layers.
# These are densely-connected, or fully-connected, neural layers.
# The first dense layer has 128 nodes (or neurons).
# The second (and last) layer is a 10-node softmax layer 
# -this returns an array of 10 probability scores that sum to 1.
# Each node contains a score that indicates the probability
# that the current image belongs to one of the 10 digit classes




#compile the model
# Loss function - This measures how accurate the model is during training.
#                 We want to minimize this function to "steer" the model in the right direction.
# Optimizer - This is how the model is updated based on the data it sees and its loss function.
# Metrics -Used to monitor the training and testing steps. The following example uses accuracy,
#         the fraction of the images that are correctly classified.
model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)


#Train the model
history <- model %>% fit(
  train_images, train_labels,
  epochs = 5, verbose = 2
  )
history

#evaluasi akurasi
score <- model %>% 
  evaluate(
    test_images, test_labels,
    verbose = 0
    )
score

#make preditictions
predictions <- model %>% predict(test_images)
# A prediction is an array of 10 numbers.
# These describe the "confidence" of the model 
# that the image corresponds to each of the 10 different articles of clothing
predictions[1,]
which.max(predictions[1, ]) #artinya model lebih yakin ini baju tipe ke 10


#plot several image
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}



# predict 1 image
# take care to keep the batch dimension, as this is expected by the model
img <- test_images[1, , , drop = FALSE]
dim(img)

predictions <- model %>% predict(img)
predictions
which.max(predictions)

class_pred <- model %>% predict_classes(img)
class_pred
