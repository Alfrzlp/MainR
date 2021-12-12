library(keras)
library(tidyverse)
library(doSNOW)

aksara = c("ba" ,"ca" ,"da" ,"dha","ga" ,"ha" ,"ja" ,"ka" ,
           "la" ,"ma" ,"na" ,"nga", "nya" ,"pa" ,"ra" ,"sa" ,
           "ta" ,"tha" ,"wa" ,"ya")

image_from_directory <- function(alamat, w, h, grayscale = T, ...){
  start = Sys.time()
  dir = list.files(alamat, full.names = T)
  allloc = c()
  
  # mendapatkan semua lokasi gambar
  if(str_detect(dir[1], "(png|jpg|jpeg)")) allloc = dir
  else{
    for(i in 1:length(dir)){
      allloc = c(allloc, list.files(dir[i], full.names = T, recursive = T))
    }
  }
  
  # agar tidak over gunakan core - 1
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores-1)
  registerDoSNOW(cl)
  
  # iterasi secara pararel
  hasil <- 
    foreach::foreach(i = 1:length(allloc), .packages = c("keras", "stringr"), ...) %dopar% {
      label = str_split(allloc[i], pattern = "/")[[1]]
      label = label[length(label)-1]
      
      img = image_load(allloc[i], target_size = c(w, h), grayscale = grayscale)
      img = image_to_array(img)
      img = array_reshape(img, c(1, dim(img)))
      img = img/255
      
      list(image = img, lab = label)
    }
  stopCluster(cl)
  # membuat fungsi abind1
  abind1 = function(...) abind::abind(..., along = 1)
  hasil <- list(image = do.call(abind1, map(hasil, "image")),
                label = do.call(c, map(hasil, "lab")))
  cat("Time", Sys.time()-start)
  return(hasil)
}



w = 100
h = 100

path = "D:/Datasets/CNN/Aksara Jawa/train"
data = image_from_directory(path, w = w, h = h, grayscale = F)
x_train = data[[1]] 
y_train = data[[2]] 
y_train = factor(y_train, labels = 0:19)

path2 = "D:/Datasets/CNN/Aksara Jawa/test"
data2 = image_from_directory(path2, w = w, h = h, grayscale = F)
x_test = data2[[1]] 
y_test = data2[[2]] 
y_test = factor(y_test, labels = 0:19)


dim(x_train)
dim(x_test)


# ubah dimensi label
y_train = to_categorical(y_train)
y_test = to_categorical(y_test)


dim(y_train)
dim(y_test)



# build model
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = 3,
                padding = "valid", input_shape = c(w, h, 1), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = 2) %>% 
  
  layer_conv_2d(filters = 64, kernel_size = 3,
                padding = "valid", activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = 2) %>% 
  
  layer_conv_2d(filters = 128, kernel_size = 3,
                padding = "valid", activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = 2) %>%
  
  layer_conv_2d(filters = 256, kernel_size = 3,
                padding = "valid", activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = 2) %>%
  
  layer_flatten() %>%
  layer_dense(64, activation = "relu") %>%
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
history <- model%>%
  fit(x_train,
      y_train,
      batch_size = 30,
      epoch = 65
  )

history

# evaluate
plot(history)
model %>% evaluate(x_test, y_test) 


# tabel-----------------------------------------------------------------
hasil = model %>% predict_classes(x_test)
hasil = factor(hasil, label = aksara)
actual = data2[[2]] 
table(prediksi = hasil, actual)

# dengan ggplot2
library(ggplot2)
table(prediksi = hasil, actual) %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_tile(aes(x = actual, y = prediksi, fill = Freq))+
  geom_text(aes(x = actual, y = prediksi, label = Freq), col = "white")+
  theme(legend.position = "none")



#predict beberapa gambar------------------------------------------------
nama = list.files("D:/Datasets/CNN/Aksara Jawa/predict")

gambar = image_from_directory("D:/Datasets/CNN/Aksara Jawa/predict", w, h, grayscale = T)
gambar = gambar[[1]]
dim(gambar)

indeks = 10

# nama foto
nama[indeks]

# hasil prediksi
model %>% predict_classes(gambar[indeks, , , , drop = F]) %>% 
  + 1 %>% aksara[.]






# 100
#save_model_hdf5(model, "D:/Datasets/Model/model_aksarajawa.h5")
model <- load_model_hdf5("D:/Datasets/Model/model_aksara_jawa100.h5")
summary(model)


# 64
model <- load_model_hdf5("D:/Datasets/Model/model_aksarajawa.h5")















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
    # fungsi putar 90 searah jarum jam
    rotate = function(x) return(t(apply(m, 2, rev)))
    #balik y dari besar
    m <- apply(m, 2, rev)
    m = rotate(m)
    m <- rotate(m)
    
    if(!is.na(label_test) && !is.na(hasil)){
      image(m, col = grey.colors(255), axes = FALSE)
      predicted_label <- hasil[indeks[i]]
      true_label <- label_test[indeks[i]]
      
      if(predicted_label == true_label) color <- "darkgreen"
      else color <- "red"
      
      text(0.1, 0.9, col = color, cex = 1.5, predicted_label)
    }else{
      image(m, col = grey.colors(255), axes = FALSE)
    }
    #-------------------------
  }
}

plotResults(sample(1:length(hasil), 25), x_test, data2[[2]], hasil)
