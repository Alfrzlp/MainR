library(ggplot2)
library(lattice)
library(caret)

dataset <- iris
validation_index <- createDataPartition(dataset$Species, p = 0.8, list = FALSE)
# 20% untuk validasi
validation <- dataset[-validation_index, ]
# 80% training and testing
dataset <- dataset[validation_index, ]
dim(dataset)
dim(validation)

sapply(dataset, class)
head(dataset)
tail(dataset)

levels(dataset$Species)

x <- dataset[, 1:4]
y <- dataset[, 5]
par(mfrow = c(1, 4))
for (i in 1:4) {
  boxplot(x[, i], main = names(iris)[i])
}
featurePlot(x = x, y = y, plot = "box")

plot(y)

library(ellipse)
featurePlot(x = x, y = y, plot = "ellipse")

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

#----linier alg
set.seed(7)
fit.lda <- train(Species ~ ., data = dataset, method = "lda", metric = metric, trControl = control)
fit.lda2 <- train(Species ~ ., data = dataset, method = "lda")
#----non liniear
# cart
set.seed(7)
fit.cart <- train(Species ~ ., data = dataset, method = "rpart", metric = metric, trControl = control)
# knn
set.seed(7)
fit.knn <- train(Species ~ ., data = dataset, method = "knn", metric = metric, trControl = control)

#----ADVANCE ALGORITHMS
# svm
set.seed(7)
fit.svm <- train(Species ~ ., data = dataset, method = "svmRadial", metric = metric, trControl = control)
# random forest
set.seed(7)
fit.rf <- train(Species ~ ., data = dataset, method = "rf", metric = metric, trControl = control)


result <- resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(result)
dotplot(result)
