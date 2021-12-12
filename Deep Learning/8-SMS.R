library(keras)
library(tidyverse)

data = read.csv2("D:/Datasets/sms/dataset_sms_spam_v1.csv", sep = ",")
view(data)
dim(data)
glimpse(data)

tokenizer = text_tokenizer(num_words = 300)
tokenizer %>% 
  fit_text_tokenizer(data$Teks)

## create sequances
data_seq <- texts_to_sequences(tokenizer, data$Teks)
data_seq
# awal
data$Teks[1]
# menjadi
data_seq[[1]]

## calculate training comments lengths
sms_length <- data_seq %>% 
  map(~ str_split(.x, pattern = " ", simplify = TRUE)) %>% 
  map_int(length)

## plot comments length distribution
tibble(sms_length = sms_length) %>% 
  ggplot(aes(sms_length))+
  geom_histogram(binwidth = 20)+
  theme_minimal()+
  ggtitle("Training data sms length distribution")

## define max_len
max_len = 30

## pad sequence
train <- pad_sequences(data_seq, maxlen = max_len, padding = "post")
dim(train)

label <- to_categorical(data$label)
dim(label)

indeks = sample(1:nrow(train), 1000)
x_train = train[indeks,]
y_train = label[indeks,]

x_test = train[-indeks,]
y_test = label[-indeks,]

model <- keras_model_sequential() %>% 
  # input_dim = jumlah kata yang berbeda (num_words)
  # output_dim -> 1 input dipresentasikan 50
  # misal kalau output_dim 2 maka 1 menjadi [0.1, 0.2] atau lainnya
  layer_embedding(input_dim = 300, output_dim = 60) %>%
  # 1000 x 30 menjadi 1000 x 30 x 60
  layer_dropout(rate = 0.2) %>%
  # add a Convolution1D
  layer_conv_1d(
    filters =  30, kernel_size = 3, 
    #valid no padding
    padding = "valid", activation = "relu", strides = 1) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 12, activation = "relu") %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 3, activation = "softmax")

summary(model)


model %>% compile(
  optimizer = optimizer_adam(lr = 0.001),
  loss = 'categorical_crossentropy',
  metrics = 'accuracy'
)


history <- model %>% 
  fit(x_train,
      y_train,
      epochs = 250,
      batch_size = 100,
      callbacks = callback_tensorboard("logs/run_a")
)  


library(tfruns)
tensorboard("logs/run_a")
tensorboard(latest_run())
library(tensorflow)
tensorboard("logs/run_a")



history
plot(history)

model %>% evaluate(x_test, y_test)
model2 <- load_model_hdf5("D:/Datasets/Model/model_sms.h5")
model2 %>% evaluate(x_test, y_test)

summary(model2)

# tabel
hasil = model2 %>% predict_classes(x_test)
actual = data$label[-indeks]
table(Predicted = hasil, Actual = data$label) 

# Penjelasan Label 
# 0: sms normal 
# 1: fraud atau penipuan 
# 2: promo

text.to.token = function(string){
  seq = texts_to_sequences(tokenizer, string)
  pad_sequences(seq, maxlen = 30, padding = "post") %>% return()
}

text.to.token("hai saya ridson, pak nanti saya")

# beberapa prediksi
model %>% 
  predict_classes(text.to.token("hai saya ridson, pak nanti saya makan"))

model %>% 
  predict_classes(text.to.token("selamat anda mendapatkan uang 100 juta"))

model %>% 
  predict_classes(text.to.token("promo 3gb hanya Rp 1000 berlaku 24 jam"))


# save_model_hdf5(model , "D:/Datasets/Model/model_sms.h5")
# model2 <- load_model_hdf5("D:/Datasets/Model/model_sms.h5")





# naive bayes-----------------------------------------------------------
df = data.frame(train, hasil = data[,2])
df$hasil = as.factor(df$hasil)
df = df %>% 
  mutate_at(1:30, ~as.character(.x))

# indeks = sample(1:nrow(train), 1000)
df_train = df[indeks,]
df_test = df[-indeks,]
dim(df_test)

library(e1071)
library(caret)
modelnb = naiveBayes(hasil~., data = df_train)
hasilnb = modelnb %>% predict(df_test)
hasilnb

confusionMatrix(table(hasilnb, df_test$hasil))
