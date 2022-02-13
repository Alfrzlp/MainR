library(keras)
library(tidyverse)
library(tidytext)

# read data
data <- read.csv("D:/Datasets/NLP/data_nama.csv")
dim(df)
head(df)


tokenizer <- text_tokenizer(
  num_words = 28,
  lower = T, char_level = T
)
# latih
tokenizer %>%
  fit_text_tokenizer(df$nama)


data_seq <- texts_to_sequences(tokenizer, df$nama)

# awalnya
df$nama[10000:10002]
# jadinya
data_seq[10000:10002]


# mendapatkan semua panjang nama
name_length <- sapply(data_seq, length)
name_length

# plot
data.frame(name_length) %>%
  group_by(name_length) %>%
  count() %>%
  ggplot() +
  geom_col(aes(x = name_length, y = n)) +
  theme_minimal() +
  ggtitle("Name length distribution")

max_len <- 30

data <- pad_sequences(data_seq, maxlen = max_len, padding = "post")
label <- df$jenis_kelamin

# ambil subset
set.seed(1927)
indeks <- sample(1:nrow(df), 0.8 * nrow(df))


x_train <- data[indeks, ]
y_train <- label[indeks]

x_test <- data[-indeks, ]
y_test <- label[-indeks]


dim(x_train)
dim(x_test)

dim(y_train)
dim(y_test)


# build model
model <- keras_model_sequential() %>%
  # input dim = num words
  layer_embedding(input_dim = 27, output_dim = 32) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(
    filters = 64, kernel_size = 2,
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  layer_max_pooling_1d(2) %>%
  layer_conv_1d(
    filters = 128, kernel_size = 2,
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  layer_max_pooling_1d(2) %>%
  layer_lstm(256) %>%
  layer_dropout(0.5) %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

# 70
history <- model %>% fit(
  x_train, y_train,
  epochs = 70,
  batch_size = 32,
  validation_split = 0.2
)

history
model %>% evaluate(x_test, y_test)
plot(history)

model2 %>% evaluate(x_test, y_test)
model3 %>% evaluate(x_test, y_test)

# Model per huruf
save_model_hdf5(model, "D:/Datasets/Model/namegender.h5")
save_text_tokenizer(tokenizer, "D:/Datasets/Model/namegender.pickle")

# simpan token
# save_text_tokenizer(tokenizer, "D:/Datasets/Model/new_gender")
token <- load_text_tokenizer("D:/Datasets/Model/namegender.pickle")



hasil <- model %>% predict_classes(x_test)
actual <- df$jenis_kelamin[-indeks]

table(prediksi = hasil, actual)

# fungsi
text.to.token <- function(string) {
  seq <- texts_to_sequences(tokenizer, string)
  pad_sequences(seq, maxlen = 32, padding = "post") %>% return()
}


# laki 1
# perempuan 0
model %>% predict_classes(text.to.token("riza"))
model %>% predict_classes(text.to.token("ninda"))
model %>% predict_classes(text.to.token("ainul"))
model %>% predict_classes(text.to.token("david"))
model %>% predict_classes(text.to.token("ashita"))
model %>% predict_classes(text.to.token("bernica"))
model %>% predict_classes(text.to.token("desi"))
model %>% predict_classes(text.to.token("risalah"))
model %>% predict_classes(text.to.token("fredika"))
model %>% predict_classes(text.to.token("aji"))
