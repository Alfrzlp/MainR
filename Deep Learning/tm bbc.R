library(tidyverse)
library(tidytext)
library(keras)
library(tm)

df <- read.csv("D:/Datasets/NLP/bbc-text.csv")
dim(df)
head(df, 2)

set.seed(1)
corpus <- VCorpus(VectorSource(df$text))
summary(corpus)

corpus[[1]]$content

corpus <- corpus %>%
  tm_map(content_transformer(removePunctuation)) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(content_transformer(removePunctuation)) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)

corpus[[1]]$content

dtm <- DocumentTermMatrix(corpus)

inspect(dtm)
dtm <- removeSparseTerms(dtm, 0.95)
dtm

data <- as.matrix(dtm)
data

label <- df$category
unique(label)
label <- factor(label, labels = 0:4)
label <- to_categorical(label)


# indeks = sample(1:nrow(data), 0.8*nrow(data))
x_train <- data[indeks, ]
y_train <- label[indeks, ]

x_test <- data[-indeks, ]
y_test <- label[-indeks, ]

model <- keras_model_sequential() %>%
  layer_dense(30, activation = "relu", input_shape = c(ncol(data))) %>%
  layer_dropout(0.2) %>%
  layer_dense(15, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(5, activation = "softmax")


summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2
)

history
model %>% evaluate(x_test, y_test)
model %>% evaluate(data, label)




# cara 2------------------------------------------------------------------
df %>%
  unnest_tokens(kata, text) %>%
  group_by(kata) %>%
  count() %>%
  nrow()

tokenizer <- text_tokenizer(num_words = 30460)
tokenizer %>%
  fit_text_tokenizer(df$text)

data_seq <- texts_to_sequences(tokenizer, df$text)

# awalnya
df$text[1]
# jadinya
data_seq[1]


length <- sapply(data_seq, length)
length

max(sapply(data_seq, max))

# plot
data.frame(length) %>%
  group_by(length) %>%
  count() %>%
  ggplot() +
  geom_col(aes(x = length, y = n)) +
  theme_minimal() +
  ggtitle("News length distribution")

max_len <- 1000

data2 <- pad_sequences(data_seq, maxlen = max_len, padding = "post")

label2 <- df$category
label2 <- factor(label2, labels = 0:4) %>% to_categorical()

x_train2 <- data2[indeks, ]
y_train2 <- label2[indeks, ]

x_test2 <- data2[-indeks, ]
y_test2 <- label2[-indeks, ]


dim(x_train2)
dim(x_test2)

dim(y_train2)
dim(y_test2)

# build model
model2 <- keras_model_sequential() %>%
  # input dim = num words
  layer_embedding(input_dim = 30460, output_dim = 20) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(
    filters = 32, kernel_size = 2,
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(32, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 5, activation = "softmax")


summary(model2)

model2 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

history2 <- model2 %>% fit(
  x_train2, y_train2,
  epochs = 30,
  batch_size = 32,
  validation_split = 0.2
)

history2
model2 %>% evaluate(x_test2, y_test2)
model2 %>% evaluate(data2, label2)
