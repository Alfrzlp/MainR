library(keras)
library(foreach)
library(purrr)
library(doSNOW) # doparalel masih kalah

library(stringr)
library(ggplot2)
# library(doParallel)

aksara <- c(
  "ba", "ca", "da", "dha", "ga", "ha", "ja", "ka",
  "la", "ma", "na", "nga", "nya", "pa", "ra", "sa",
  "ta", "tha", "wa", "ya"
)


image_from_directory <- function(alamat, w, h, grayscale = T, ...) {
  start <- Sys.time()
  dir <- list.files(alamat, full.names = T)
  allloc <- c()

  # mendapatkan semua lokasi gambar
  if (str_detect(dir[1], "(png|jpg|jpeg)")) {
    allloc <- dir
  } else {
    for (i in 1:length(dir)) {
      allloc <- c(allloc, list.files(dir[i], full.names = T, recursive = T))
    }
  }

  # agar tidak over gunakan core - 1
  cores <- parallel::detectCores()
  cl <- makeSOCKcluster(cores - 1)
  registerDoSNOW(cl)

  # iterasi secara pararel
  hasil <-
    foreach::foreach(i = 1:length(allloc), .packages = c("keras", "stringr"), ...) %dopar% {
      label <- str_split(allloc[i], pattern = "/")[[1]]
      label <- label[length(label) - 1]

      img <- image_load(allloc[i], target_size = c(w, h), grayscale = grayscale)
      img <- image_to_array(img)
      img <- array_reshape(img, c(1, dim(img)))
      img <- img / 255

      list(image = img, lab = label)
    }
  stopCluster(cl)
  # membuat fungsi abind1
  abind1 <- function(...) abind::abind(..., along = 1)
  hasil <- list(
    image = do.call(abind1, map(hasil, "image")),
    label = do.call(c, map(hasil, "lab"))
  )
  cat("Time  :", Sys.time() - start)
  return(hasil)
}



path <- "D:/Datasets/CNN/Aksara Jawa/train"

start <- Sys.time()
train <- image_from_directory(path, 32, 32)
print(Sys.time() - start)



path2 <- "D:/Datasets/CNN/Aksara Jawa/test"

start <- Sys.time()
test <- image_from_directory(path2, 32, 32, .inorder = F)
print(Sys.time() - start)


predict <- image_from_directory("D:/Datasets/CNN/Aksara Jawa/predict", 32, 32)
predict[[2]]








dim(x_train)
dim(x_test)


# ubah dimensi label
y_train <- to_categorical(y_train)
y_test <- to_categorical(y_test)

dim(y_train)
dim(y_test)



# build model
model <- keras_model_sequential() %>%
  layer_conv_2d(
    filters = 32, kernel_size = 3,
    padding = "same", input_shape = c(32, 32, 1), activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(
    filters = 64, kernel_size = 3,
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(
    filters = 128, kernel_size = 3,
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(
    filters = 64, kernel_size = 3,
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(
    filters = 32, kernel_size = 3,
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(32, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(20, activation = "softmax")

# same, dimensi input = dimensi output
# valid, kecuali filter 1x1, dimensi input != dimensi output

summary(model)

# compile
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = "accuracy"
)

# fit
history <- model %>%
  fit(x_train,
    y_train,
    batch_size = 30,
    epoch = 50
  )

history
plot(history)
model %>% evaluate(x_test, y_test)

# tabel
hasil <- model %>% predict_classes(x_test)
actual <- data2[[2]]
table(prediksi = hasil, actual)



# predict beberapa gambar
img <- list.files("D:/Datasets/CNN/Aksara Jawa/predict", full.names = T)
gambar <- image_preprocessor(img[11], 32, 32, grayscale = T)
img[11]

# output predict clasess 0-19
model %>%
  predict_classes(gambar) %>%
  +1 %>%
  aksara[.]




# save_model_hdf5(model, "D:/Datasets/Model/model_aksarajawa.h5")
model <- load_model_hdf5("D:/Datasets/Model/model_aksarajawa.h5")

# dengan ggplot2
table(prediksi = hasil, actual) %>%
  as.data.frame() %>%
  ggplot() +
  geom_tile(aes(x = actual, y = prediksi, fill = Freq)) +
  geom_text(aes(x = actual, y = prediksi, label = Freq), col = "white") +
  theme(legend.position = "none")









pb <- txtProgressBar(1, 1000, style = 3)
for (i in 1:1000) {
  Sys.sleep(0.002)
  setTxtProgressBar(pb, i)
}
close(pb)


# make progresbar
pb <- txtProgressBar(min = 1, max = length(allloc), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

pb <- progress_bar$new(
  format = "letter = : letter [:bar] :elapsed | eta: :eta",
  total = length(allloc),
  width = 60
)

library(progress)
pb <- progress_bar$new(
  format = "letter = : letter [:bar] :elapsed | eta: :eta",
  total = 1000,
  width = 60
)
progress_letter <- rep(LETTERS[1:10], 1000)
progress <- function(n) pb$tick(tokens = list(letter = progress_letter[n]))

for (i in 1:1000) progress(i)

opts <- list(progress = progress)
library(stringr)
label <- str_split(img[2], pattern = "/")[[1]]
label <- label[length(label) - 1]
