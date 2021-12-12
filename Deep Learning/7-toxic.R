library(keras)
library(tidyverse)
library(here)
library(readxl)

## read train and test data
train_data <- read_csv(here("static/data/", "toxic_comments/train.csv"))
test_data <- read(here("static/data/", "toxic_comments/test.csv"))


train_data = read.csv2("D:/Datasets/jigsaw toxic comments/train.csv", sep = ",")
test_data = read.csv2("D:/Datasets/jigsaw toxic comments/test.csv", sep = ",")

names(train_data)
view(train_data[15:18,])

names(test_data)
head(test_data)


## define vocab size (this is parameter to play with)
vocab_size = 20000

tokenizer <- text_tokenizer(num_words = vocab_size) %>% 
  fit_text_tokenizer(train_data$comment_text)

## create sequances
train_seq <- texts_to_sequences(tokenizer, train_data$comment_text)
test_seq <- texts_to_sequences(tokenizer, test_data$comment_text)

train_seq
test_seq

# awal
train_data$comment_text[1]
# menjadi
train_seq[1]


## calculate training comments lengths
comment_length <- train_seq %>% 
  map(~ str_split(.x, pattern = " ", simplify = TRUE)) %>% 
  map_int(length)

## plot comments length distribution
tibble(comment_length = comment_length)%>% 
  ggplot(aes(comment_length))+
  geom_histogram(binwidth = 20)+
  theme_minimal()+
  ggtitle("Training data comments length distribution")

## define max_len
max_len = 200

## pad sequence
x_train <- pad_sequences(train_seq, maxlen = max_len, padding = "post")
x_test <- pad_sequences(test_seq, maxlen = max_len, padding = "post")

## extract targets columns and convert to matrix
y_train <- train_data %>% 
  select(toxic:identity_hate) %>% 
  as.matrix()

y_train

## define embedding size
emd_size = 64

## define model
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = vocab_size, output_dim = emd_size) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 6, activation = "sigmoid")

summary(model)

## specify model optimizer, loss and metrics
model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)

history <- model %>% 
  fit(x_train,
      y_train,
      epochs = 10,
      batch_size = 64,
      validation_split = 0.3,
      verbose = 0)
