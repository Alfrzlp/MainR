library(caret)

dat <- read.csv("D:/_Datasets/heart_failure_clinical_records_dataset.csv") %>%
  janitor::clean_names()
c("#FFEBCD", "#FFFFFF", "#FFFFFF")
CPCOLS <- c("#1f78b4", "burlywood1", "#e31a1c")

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point(aes(col = Species)) +
  scale_colour_manual(values = CPCOLS)
dat %>%
  glimpse()

colSums(is.na(dat))



# Heatmap cor -----------------------------------------------------------
reshape::melt(cor(dat)) %>%
  ggplot(aes(X1, X2, fill = value)) +
  geom_raster() +
  scale_fill_gradient2(
    low = "black", high = "burlywood1", mid = "red",
    midpoint = 0, limit = c(-1, 1), space = "Lab"
  ) +
  hrbrthemes::theme_ft_rc() +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 1,
    size = 12, hjust = 1
  ))


# Data Partition ----------------------------------------------------------
set.seed(1)
train.index <- createDataPartition(dat$death_event, p = .8, list = FALSE)
train <- dat[train.index, ]
xtest <- dat[-train.index, -13]
ytest <- dat[-train.index, 13]

table(train$death_event)
table(ytest)


# Model -------------------------------------------------------------------
(m1 <- glm(death_event ~ ., train, family = binomial))
summary(m1)

varImp(m1)

# Akurasi train
train_prob <- predict(m1, train[, -13], type = "response")
cmtrain <- table(train$death_event, ifelse(train_prob > 0.65, 1, 0))
confusionMatrix(cmtrain)


# Akurasi test
test_prob <- predict(m1, xtest, type = "response")
ypred <- ifelse(test_prob > 0.65, 1, 0)
cmtest <- table(ytest, ypred)
(cm <- confusionMatrix(cmtest))


library(pROC)
rROC <- roc(ytest ~ test_prob,
  plot = TRUE, print.auc = TRUE,
  col = "black", lwd = 4, legacy.axes = TRUE, main = "ROC Curves"
)
