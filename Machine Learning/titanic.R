library(keras)
library(caret)
library(tidyverse)

train <- read.csv("D:/_Datasets/Titanic/train.csv") %>%
  janitor::clean_names()
sub <- read.csv("D:/_Datasets/Titanic/test.csv") %>%
  janitor::clean_names()
jawaban <- read.csv("D:/_Datasets/Titanic/jawaban.csv") %>%
  janitor::clean_names()

head(train)
head(sub)
# 0.78947

colSums(is.na(train))
colSums(is.na(sub))

unique(train$Sex)
unique(train$Embarked)

Age <- median(train$age, na.rm = T)

train <-
  train %>%
  select(-name, -ticket, -cabin) %>%
  mutate(
    sex = factor(sex, levels = c("male", "female"), labels = 0:1, ordered = F),
    embarked = factor(embarked,
      levels = c("S", "Q", "C"),
      labels = 0:2, ordered = T
    )
  ) %>%
  replace_na(list(
    age = Age,
    embarked = 0
  ))

sub <- sub %>%
  janitor::clean_names() %>%
  select(-name, -ticket, -cabin) %>%
  mutate(
    sex = factor(sex,
      levels = c("male", "female"),
      labels = 0:1, ordered = F
    ),
    embarked = factor(embarked,
      levels = c("S", "Q", "C"),
      labels = 0:2, ordered = T
    )
  ) %>%
  replace_na(list(
    age = Age,
    embarked = 0
  ))

colSums(is.na(train))
colSums(is.na(sub))

set.seed(1)
index <- createDataPartition(train$survived, p = 0.8, groups = T, list = F)
xtrain <- train[index, ]
xtest <- train[-index, ]

dim(xtrain)


# Random Forest -----------------------------------------------------------

library(rpart)
library(rpart.plot)

control <- rpart.control(minsplit = 8, minbucket = 2, maxdepth = 3, cp = 0)

fit <- rpart(survived ~ ., train, method = "class", control = control)
rpart.plot(fit, extra = 106)

predcited <- predict(fit, xtest, type = "class")
predcited
tab <- table(xtest$survived, predcited)
confusionMatrix(tab)

predict(fit, sub, "class")

sub %>%
  select(PassengerId = passenger_id) %>%
  mutate(Survived = predict(fit, sub, "class")) %>%
  write.csv("D:/Sampah/hasil.csv", row.names = F)


# Logistic Regression -----------------------------------------------------

xtrain

logit <- glm(survived ~ ., train, family = binomial(link = "logit"))
lr_result <- predict(logit, xtest, type = "response")

tab <- table(xtest$survived, ifelse(lr_result > 0.65, 1, 0))
confusionMatrix(tab)

# jawaban
lr_sub <- predict(logit, sub, "response")
tab <- table(jawaban$survived, ifelse(lr_sub > 0.65, 1, 0))
confusionMatrix(tab)

sub %>%
  select(PassengerId = passenger_id) %>%
  mutate(Survived = ifelse(lr_sub > 0.65, 1, 0)) %>%
  write.csv("D:/Sampah/hasil.csv", row.names = F)


# Naive Bayes -------------------------------------------------------------

library(e1071)
x_train <- xtrain %>%
  mutate_all(~ as.factor(.x))
x_test <- xtest %>%
  mutate_all(~ as.factor(.x))

nb_model <- naiveBayes(survived ~ ., data = x_train)
nb_predict <- predict(nb_model, newdata = x_test)
nb_predict
tab <- table(xtest$survived, nb_predict)
confusionMatrix(tab)



# Deep Learning -----------------------------------------------------------

x_train <- xtrain[, -2] %>% data.matrix(rownames.force = F)
x_train
y_train <- xtrain$survived


x_test <- xtest[, -2] %>% data.matrix(rownames.force = F)
y_test <- xtest$survived


model <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(ncol(x_train))) %>%
  layer_dense(units = 3, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid") # sigmoid biasanya 0 1

summary(model)


model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 50, batch_size = 20, # data dimasukkan 20, 20 dst... sampai 120
)

history
plot(history)

# evaluasi
model %>% evaluate(x_test, y_test)

# predict some data
model %>% predict_classes(x_test)
