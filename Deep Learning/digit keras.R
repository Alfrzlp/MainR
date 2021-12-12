library(keras)

mnist <- dataset_mnist()
class(mnist)
View(mnist)

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

str(mnist$train)
View(x_train)
#========================================================================
# reshape
# x_train <- array_reshape(x_train, c(nrow(x_train), 784))
# View(x_train)
# x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# gapake array shape kalau sudah pakai layer flatten

# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

#defining model
model <- keras_model_sequential() 
model %>% 
  layer_flatten(input_shape = c(28, 28)) %>% #menjadikan 1d array 784
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
#plot(history)

# jika training menghasilkan accu yang bagus
# namun ketika dites dengan data baru ternyata acc rendah
# maka ini overfitting
history
model %>% evaluate(x_test, y_test)

hasil = model %>% predict_classes(x_test)
hasil


#========================================================================
# test dalam ukuran 10000 x 784
# label_test dan hasil dalam 1 x 10000
plotResults <- function(indeks, test, label_test=NA, hasil=NA) {
  x <- ceiling(sqrt(length(indeks)))
  par(mfrow = c(x,x), mar = c(.1,.1,.1,.1))
  
  for (i in 1:length(indeks)) {
    #-------------------------
    if(length(dim(test)) == 4){
      m = test[indeks[i], , , ,drop = F]
    }else{
      #mengembalikan size dari 1 x 784 menjadi 28 x 28
      m <- matrix(test[indeks[i],], nrow = 28, byrow = TRUE) 
    }
    #-------------------------
    #balik y dari besar
    m <- apply(m, 2, rev)
    
    if(!is.na(label_test) && !is.na(hasil)){
      image(t(m), col = grey.colors(255), axes = FALSE)
      predicted_label <- hasil[indeks[i]]
      true_label <- label_test[indeks[i]]
      
      if(predicted_label == true_label) color <- "green"
      else color <- "red"
      
      text(0.1, 0.9, col = color, cex = 1.5, predicted_label)
    }else{
      image(t(m), col = grey.colors(255), axes = FALSE)
    }
    #-------------------------
  }
}
#=======================================================================
y_test <- mnist$test$y
# x_test <- array_reshape(x_test, c(nrow(x_test), 784))

plotResults(937:1000, x_test, y_test, hasil)
plotResults(50:78, x_test)



#bentuk nya
hasil
y_test

model %>% predict_classes(x_test[1:2, ])

x_test[1, ,] 

length(dim(x_test))

library(tidyr)
library(ggplot2)
#data matriks jadi df
image_1 <- as.data.frame(x_train[2, , ])
image_1
colnames(image_1) <- seq_len(ncol(image_1))
image_1

image_1$y <- seq_len(nrow(image_1))
image_1

image_1 <- gather(image_1, "x", "value", -y)
image_1

#x masih karakter
str(image_1)
image_1$x <- as.integer(image_1$x)
image_1

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA)+
  #ubah y dari beasr ke kecil
  scale_y_reverse() +
  theme_minimal() +
  #hapus garis2 di plot
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())+
  theme(aspect.ratio = 1)
