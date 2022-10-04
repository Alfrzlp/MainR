library(dplyr)
library(yardstick)

dat <- data.frame(
  age = c("<=30","<=30","31…40",">40",">40",">40","31…40","<=30","<=30",">40","<=30", "31…40","31…40",">40"),
  income = c("high","high","high","medium","low","low","low","medium","low","medium","medium","medium","high","medium"),
  student = c("no","no","no","no","yes","yes","yes","no","yes","yes","yes","no","yes","no"),
  credit_rating = c("fair","excellent","fair","fair","fair","excellent","excellent","fair","fair","fair","excellent","excellent","fair","excellent"),
  buys_comp = c("no","no","yes","yes","yes","no","yes","no","yes","yes","yes","yes","yes","no")
)

# mengubah tipe semua variabel menjadi factor
dat <- dat %>% mutate_all(~ as.factor(.x)) 
glimpse(dat)

train <- dat[1:10, ]
test <- dat[11:14, ]


# Decision tree -----------------------------------------------------------
library(rpart)
library(rpart.plot)

dt_model <- rpart(buys_comp ~ ., data = train, control = rpart.control(minsplit = 2))
dt_model

# plot
rpart.plot(dt_model, type = 3)

# prediksi
pred_dt <- predict(dt_model, test, type = 'class')
pred_dt

# confusion matrix
cm <- table(actual = test$buys_comp, prediction = pred_dt)
cm
# akurasi
sum(diag(cm))/sum(cm)


all_metric <- metric_set(accuracy, precision, sensitivity, f_meas)
all_metric(test, truth = buys_comp, estimate = pred_dt)


# dengan package party
library(party)
library(dplyr)

dat <- dat %>% mutate_all(~ as.factor(.x)) 
dctr <- ctree(
  buys_comp ~ ., 
  data = train, controls = ctree_control(minbucket = 1, mincriterion = 0, minsplit = 2)
)
dctr
plot(dctr)

predict(dctr, test)





# Naive Bayes -------------------------------------------------------------
library(e1071)
nb_model <- naiveBayes(buys_comp ~ ., data = train)
nb_model

pred_nb <- predict(nb_model, test, type = 'class')
pred_nb

# confusion matrix
cm <- table(actual = test$buys_comp, prediction = pred_nb)
cm
# akurasi manual
sum(diag(cm))/sum(cm)


all_metric(test, truth = buys_comp, estimate = pred_nb)



# prediksi data baru --------------------------------------------------------------------
df_pred <- data.frame(
  age = ">40",
  income = "high",
  student = "no",
  credit_rating = "excellent" 
)

# Dengan decision tree
predict(dt_model, df_pred, type = "class")
# Dengan Naive Bayes
predict(nb_model, df_pred, type = "class")



# -------------------------------------------------------------------------
y <- train$buys_comp
x <- train[-5]
.get_feature_split(y, x, .entropy, min_samples_leaf = 0)


y <- y[train$age != "<=30"]
x <- x[train$age != "<=30",]

.get_feature_split(y, x, .entropy, min_samples_leaf = 0)
.entropy(train$buys_comp) - 2/10*.entropy(train$buys_comp[train$age == "31…40"]) - 8/10*.entropy(train$buys_comp[train$age != "31…40"])


y <- y[x$income != "low"]
x <- x[x$income != "low",]
.get_feature_split(y, x, .entropy, min_samples_leaf = 0)


y <- y[x$credit_rating == "excellent"]
x <- x[x$credit_rating == "excellent",]
.get_feature_split(y, x, .entropy, min_samples_leaf = 0)

