library(readtext)
library(tidyverse)
library(keras)

train <- readtext("D:/Datasets/stackoverflow/train")
test <- readtext("D:/Datasets/stackoverflow/test")

dim(train)
dim(test)

# lihat data
head(train)
view(train)


# data cleaning
train <- train %>%
  mutate(
    doc_id = str_extract(doc_id, "(csharp|Java|javascript|python)"),
    doc_id = case_when(
      doc_id == "csharp" ~ 0, doc_id == "Java" ~ 1,
      doc_id == "javascript" ~ 2, doc_id == "python" ~ 3
    )
  )

test <- test %>%
  mutate(
    doc_id = str_extract(doc_id, "(csharp|Java|javascript|python)"),
    doc_id = case_when(
      doc_id == "csharp" ~ 0, doc_id == "Java" ~ 1,
      doc_id == "javascript" ~ 2, doc_id == "python" ~ 3
    )
  )
# melihat beberapa isi data
sample_n(train, 5)
sample_n(test, 5)

#
tokenizer <- text_tokenizer(num_words = 20000)
tokenizer %>%
  fit_text_tokenizer(train$text)

# ubah kata jadi angka
data_seq_train <- texts_to_sequences(tokenizer, train$text)
data_seq_test <- texts_to_sequences(tokenizer, test$text)


# awal
train$text[2]
# jadinya
data_seq_train[[2]]

# ambil panjang setiap question
question_length <- sapply(data_seq_train, length)

# Plot distribusi panjang pertanyaan
data.frame(question_length) %>%
  ggplot() +
  geom_histogram(aes(x = question_length), binwidth = 20) +
  theme_minimal() +
  ggtitle("Training data question length distribution")


sapply(data_seq_test, length) %>%
  data.frame(x = .) %>%
  ggplot() +
  geom_histogram(aes(x = x), binwidth = 20) +
  theme_minimal() +
  ggtitle("Testing data question length distribution")


max(question_length)
question_length[question_length >= 1000]


# max len
max_len <- 500

# make train and test data
x_train <- pad_sequences(data_seq_train, maxlen = max_len, padding = "post")
x_test <- pad_sequences(data_seq_test, maxlen = max_len, padding = "post")
dim(x_train)
dim(x_test)

y_train <- to_categorical(train$doc_id)
y_test <- to_categorical(test$doc_id)
dim(y_test)
dim(y_test)


# membuat model
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 20000, output_dim = 100) %>%
  layer_dropout(rate = 0.2) %>%
  layer_conv_1d(
    filters = 50, kernel_size = 3,
    # valid no padding
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = 12, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 4, activation = "softmax")

summary(model)

# loss = jarak antara nilai target dan prediksi untuk setiap sample
# optimizer = mekanisme yang digunakan untuk mengupdate bobot berdasarkan
# loss function
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = "accuracy"
)


## fit
history <- model %>%
  fit(x_train, y_train,
    epochs = 5,
    batch_size = 64,
    verbose = 1
  )

history
model %>% evaluate(x_test, y_test)

# tabel
hasil <- model %>% predict_classes(x_test)
actual <- test$doc_id
table(Predicted = hasil, Actual = actual)



# 0.1502469    0.8852500
# save_model_hdf5(model , "D:/Datasets/Model/model_stackoverflow.h5")
model <- load_model_hdf5("D:/Datasets/Model/model_stackoverflow.h5")
