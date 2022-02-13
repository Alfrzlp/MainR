library(katadasaR)
library(textclean)
library(tidyverse)
library(keras)

df <- read.csv(r"(D:\Datasets\NLP\lazada\20191002-reviews.csv)")
df %>%
  glimpse()

stemming <- function(kalimat) {
  text <- sapply(str_split(kalimat, "\\s")[[1]], katadasaR)
  return(paste(text, collapse = " "))
}

katadasar("menyakitkan")

kalimat <- "menidurkan membuang membuat terkadang menyakitkan kadang juga menyenangkan"
stemming(kalimat)




df <- df %>%
  filter(reviewContent != reviewTitle) %>%
  select(reviewContent, reviewTitle, rating)

dim(df)

df %>%
  sample_n(5)





df <- df %>%
  mutate(
    Tweet = tolower(Tweet),
    Tweet = gsub("\\n", "\\s", Tweet),
    Tweet = replace_url(Tweet),
    Tweet = replace_html(Tweet),
    Tweet = replace_tag(Tweet, pattern = "@([A-Za-z0-9_]+)", replacement = ""),
    Tweet = replace_hash(Tweet, pattern = "#([A-Za-z0-9_]+)", replacement = ""),
    Tweet = replace_emoji(Tweet),
    Tweet = strip(Tweet)
  )

df[3:10, ]

df <- df %>%
  mutate(Tweet = sapply(df$Tweet, katadasaR, USE.NAMES = F))


tokenizer <- text_tokenizer(num_words = 5000)
tokenizer %>%
  fit_text_tokenizer(df$Tweet)

data_seq <- texts_to_sequences(tokenizer, df$Tweet)

# awalnya
df$Tweet[1:3]
# jadinya
data_seq[1:3]


# mendapatkan semua panjang nama
tweet_length <- sapply(data_seq, length)
tweet_length

# plot
data.frame(tweet_length) %>%
  group_by(tweet_length) %>%
  count() %>%
  ggplot() +
  geom_col(aes(x = tweet_length, y = n)) +
  theme_minimal() +
  ggtitle("Tweet length distribution")

max_len <- 21

data <- pad_sequences(data_seq, maxlen = max_len, padding = "post")
label <- df$sentimen
label <- replace(label, label == -1, 2)
label <- to_categorical(label)
# 0 netral
# 1 positif
# 2 negatif


set.seed(1927)
indeks <- sample(1:nrow(df), 0.8 * nrow(df))

x_train <- data[indeks, ]
y_train <- label[indeks, ]

x_test <- data[-indeks, ]
y_test <- label[-indeks, ]


dim(x_train)
dim(x_test)

dim(y_train)
dim(y_test)



model <- keras_model_sequential() %>%
  # input dim = num words
  layer_embedding(input_dim = 5000, output_dim = 50) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(
    filters = 50, kernel_size = 2,
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  layer_max_pooling_1d(2) %>%
  layer_lstm(128) %>%
  layer_dropout(0.5) %>%
  layer_dense(64, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 3, activation = "softmax")

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

# 70
history <- model %>% fit(
  x_train, y_train,
  epochs = 30,
  batch_size = 32,
  validation_split = 0.2
)


model %>% evaluate(x_test, y_test)































library(tm)

df <- read.csv(r"(D:\Datasets\NLP\twitter\Indonesian Sentiment Twitter Dataset Labeled.csv)",
  sep = "\t"
)
stopword <- read.csv(r"(D:\Datasets\stopwordindo.csv)") %>% pull()

corpus <- VCorpus(VectorSource(df$Tweet))
summary(corpus)

corpus[[1]]$content

corpus <- corpus %>%
  tm_map(content_transformer(removePunctuation)) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopword) %>%
  tm_map(content_transformer(removePunctuation)) %>%
  # tm_map(stemDocument) %>%
  tm_map(stripWhitespace)
tm_map(stemming)

corpus[[1]]$content

dtm <- DocumentTermMatrix(corpus)

inspect(dtm)
dtm <- removeSparseTerms(dtm, 0.999)
dtm

data <- as.matrix(dtm)
dim(data)
data

label <- df$sentimen
label <- replace(label, label == -1, 2)
label <- to_categorical(label)

x_train <- data[indeks, ]
y_train <- label[indeks, ]

x_test <- data[-indeks, ]
y_test <- label[-indeks, ]

model <- keras_model_sequential() %>%
  layer_dense(30, activation = "relu", input_shape = c(ncol(data))) %>%
  layer_dropout(0.2) %>%
  layer_dense(15, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(3, activation = "softmax")


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

model %>% evaluate(x_test, y_test)
