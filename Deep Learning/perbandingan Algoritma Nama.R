library(keras)
library(tidyverse)

df <- readxl::read_xlsx(r"(D:\Datasets\Frame Mahasiswa STIS.xlsx)")
df <- df %>%
  select(NAMA, JK)

# load model and tokenizer
model <- load_model_hdf5("D:/Datasets/Model/NLP/nama.h5")
tokenizer <- load_text_tokenizer("D:/Datasets/Model/NLP/token_nama.pickle")


# membuat fungsi
nama_2vec <- function(string, max_len = 32) {
  seq <- texts_to_sequences(tokenizer, string)
  pad_sequences(seq, maxlen = max_len, padding = "post") %>% return()
}

prediksi <- function(nama) {
  nama <- nama_2vec(nama)
  hasil <- model %>% predict_classes(nama)

  if (hasil[1] == 0) {
    jk <- "P"
  } else {
    jk <- "L"
  }

  return(jk)
}
# testing
prediksi("rara")


# run
hasil <- c()
for (i in 1:nrow(df)) {
  hasil <- c(hasil, prediksi(df$NAMA[i]))
  cat(i, "done \n")
}

# cek hasil
unique(hasil)
# acc
table(df$JK, hasil) %>%
  diag() %>%
  sum() * 100 / nrow(df)

# conf matrix
table(df$JK, hasil)
# model 3   92.56116
# model 2   92.56116
# model 1   97.75337

summary(model)

df %>%
  mutate(pred = hasil) %>%
  filter(pred != JK)
