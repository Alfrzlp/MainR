library(keras)
library(tensorflow)

# lihat daftar gambar
gambar <- list.files("D:/Datasets/CNN with R/sebelum diresize")
gambar

# set width
w <- 100
# set height
h <- 100

library(opencv)
library(EBImage)

for (i in 1:length(gambar)) {
  imgname <- gambar[i]
  img <- ocv_read(paste0("D:/Datasets/CNN with R/sebelum diresize/", imgname))
  img_resized <- ocv_resize(img, w = w, h = h)
  ocv_write(img_resized, paste0("D:/Datasets/CNN with R/setelah diresize/", imgname))
  print(paste(i, "done", sep = " "))
}

for (i in 1:length(gambar)) {
  imgname <- gambar[i]
  img <- readImage(paste0("D:/Datasets/CNN with R/sebelum diresize/", imgname))
  # resize image to 100x100
  img_resized <- resize(img, w = w, h = h)
  path <- paste0("D:/Datasets/CNN with R/setelah diresize/", imgname)
  # save image
  writeImage(img_resized, path, quality = 70)
  print(paste(i, "done", sep = " "))
}

gambar2 <- list.files("D:/Datasets/CNN with R/setelah diresize")
gambar2
gambar2 <- paste0("D:/Datasets/CNN with R/setelah diresize/", gambar2)
gambar2

gambar2 <- lapply(gambar2, readImage)
str(gambar2)

display(gambar2[[4]])
dim(gambar2[[4]])

# crate train and test data
train <- gambar2[c(1:80, 101:180)]
test <- gambar2[c(81:100, 181:200)]


train[[5]]
display(train[[5]])
write.csv(train[[5]], "E://test/datatrain.csv")

par(mfrow = c(2, 4))
for (i in 1:160) plot(train[[i]])

# resize
for (i in 1:160) {
  train[[i]] <- resize(train[[i]], 32, 32)
}
for (i in 1:40) {
  test[[i]] <- resize(test[[i]], 32, 32)
}
str(train)
str(test)

# combine
train <- combine(train)
x <- tile(train, 20)
display(x, title = "gambar")
dim(train)

test <- combine(test)
y <- tile(test, 10)
display(y, title = "gambar2")
dim(test)

dim(train)

# mengubah urutan dimensi dari 1 2 3 4
train <- aperm(train, c(4, 1, 2, 3))
test <- aperm(test, c(4, 1, 2, 3))
dim(train)
dim(test)

# Response
trainy <- c(rep(0, 80), rep(1, 80))
testy <- c(rep(0, 20), rep(1, 20))
trainy
testy

# On hot encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)
trainLabels
testLabels


# membuat model
model <- keras_model_sequential()
model %>%
  layer_conv_2d(
    filters = 32,
    kernel_size = c(3, 3),
    activation = "relu",
    input_shape = c(32, 32, 3)
  ) %>%
  layer_conv_2d(
    filters = 32,
    kernel_size = c(3, 3),
    activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.01) %>%
  layer_conv_2d(
    filters = 64,
    kernel_size = c(3, 3),
    activation = "relu"
  ) %>%
  layer_conv_2d(
    filters = 64,
    kernel_size = c(3, 3),
    activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.01) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.01) %>%
  layer_dense(units = 2, activation = "softmax") %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_sgd(
      lr = 0.01,
      decay = 1e-06,
      momentum = 0.9,
      nesterov = T
    ),
    metrics = c("accuracy")
  )
summary(model)

proses <- model %>%
  fit(train,
    trainLabels,
    epoch = 30,
    batch_size = 32,
    validation_split = 0.2
  )

proses
plot(proses)

# Evaluation & Prediction - train data
model %>% evaluate(train, trainLabels)
pred <- model %>% predict_classes(train)
table(Predicted = pred, Actual = trainy)

prob <- model %>% predict_proba(train)
cbind(prob, Predicted_class = pred, Actual = trainy)

# Evaluation & Prediction - test data
model %>% evaluate(test, testLabels)
pred <- model %>% predict_classes(test)
table(Predicted = pred, Actual = testy)

prob <- model %>% predict_proba(test)
cbind(prob, Predicted_class = pred, Actual = testy)






# save model
save_model_hdf5(model, filepath = "E://hasiltest.hdf5")

# test Model
setwd("E:/uji/")

model <- load_model_hdf5(filepath = "E://hasiltest.hdf5")
images <- list.files()
images
summary(images)

images <- lapply(images, readImage)
head(images)
display(images[[3]])

# Get the image as a matrix
test <- images

for (i in 1:20) {
  test[[i]] <- resize(test[[i]], 32, 32)
}
for (i in 1:20) {
  test[[i]] <- toRGB(test[[i]])
}

fixx <- combine(test)
y <- tile(fixx, 5)
display(y, title = "Pics")

str(fixx)
Uji <- aperm(fixx, c(4, 1, 2, 3))
str(Uji)

testy <- c(rep(0, 10), rep(1, 10))

# One hot encoding
testLabels <- to_categorical(testy)

pred <- model %>% predict_classes(Uji)
table(Predicted = pred, Actual = testy)

model %>% evaluate(Uji, testLabels)

prob <- model %>% predict_proba(Uji)
prob
colnames(prob) <- c("Mobil", "Motor")
Duhan <- cbind(prob, Predicted_class = pred, Actual = testy)
Duhan














library(EBImage)
image_from_directory <- function(alamat, w, h, grayscale = T) {
  start <- Sys.time()
  dir <- list.files(alamat)
  allloc <- c()
  label <- c()

  # mendapatkan semua lokasi gambar
  if (str_detect(dir[1], "(png|jpg|jpeg)")) {
    allloc <- dir
  } else {
    for (i in 1:length(dir)) {
      isi <- list.files(paste0(alamat, "/", dir[i]), full.names = T, recursive = T)
      allloc <- c(allloc, isi)
      label <- c(label, rep(dir[i], length(isi)))
    }
  }
  # make progress bar
  pb <- txtProgressBar(1, length(allloc), style = 3)

  images <- lapply(allloc, readImage)
  for (i in 1:length(allloc)) {
    images[[i]] <- resize(images[[i]], w, h)
    images[[i]] <- toRGB(images[[i]])
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # gabung semua image
  images <- combine(images)
  # ubah urutan dimensi
  images <- aperm(images, c(4, 1, 2, 3))
  cat("Time  :", Sys.time() - start)
  return(list(images, label))
}

path <- "D:/Datasets/CNN/Aksara Jawa/train"
hasil <- image_from_directory(path, 32, 32)
hasil[[1]] %>% dim()
hasil[[2]] %>% length()
?readImage()
