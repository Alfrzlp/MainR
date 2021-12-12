library(caret)
library(glmnet)
# alt shift atas/bawah

dt <- read.csv('D:/_Datasets/diabetes.csv')
dim(dt)

# Number of times pregnant
# Plasma glucose concentration a 2 hours in an oral glucose tolerance test
# Diastolic blood pressure (mm Hg)
# Triceps skin fold thickness (mm)
# 2-Hour serum insulin (mu U/ml)


dt %>% 
  group_by(diabetes) %>% 
  count()

# Tidak ada kolom NA namun kolom 0
colSums(is.na(dt))
colSums(dt[,-9] == 0)


# Missing Value -----------------------------------------------------------
library(mice)

dat <- dt %>% 
  mutate_at(1:8, ~ifelse(.x == 0, NA, .x))
colSums(is.na(dat))

miceMod <- mice(dat, method = 'rf', seed = 1)  
dat <- complete(miceMod)
colSums(is.na(dat))


ggplot(dat, aes(x = ), group = diabetes)+
  geom_point()

# Data Partition ----------------------------------------------------------
set.seed(1)
# train.index <- createDataPartition(dat$diabetes, p = .8, list=FALSE)
train <- dat[train.index,]
xtest <- dat[-train.index, -9]
ytest <- dat[-train.index, 9]

table(train$diabetes)
table(ytest)


# Unregularized Model -----------------------------------------------------
m1 <- glm(diabetes~(.)^2, train, family = binomial) %>% 
  MASS::stepAIC()
summary(m1)

# Akurasi train
train_prob <- predict(m1, train[,-9], type = 'response')
cmtrain <- table(train$diabetes, ifelse(train_prob > 0.65, 1, 0))
confusionMatrix(cmtrain)


# Akurasi test
test_prob <- predict(m1, xtest, type = 'response')
ypred <- ifelse(test_prob > 0.65, 1, 0)
cmtest <- table(ytest, ypred)
(cm <- confusionMatrix(cmtest))

# Gmean
# Sebagai indikator yang dapat mengakomodir akurasi pada kelas positif
# (sensitivity) dan kelas negatif (specificity) dalam kasus imbalanced data
sqrt(cm$byClass['Sensitivity']*cm$byClass['Specificity']) %>% 
  `names<-`('G mean')


library(pROC)
rROC <- roc(ytest ~ test_prob, plot=TRUE, print.auc=TRUE,
            col="black", lwd =4, legacy.axes=TRUE,main="ROC Curves")
# AUC = luas daerah dibawah kurva ROC
# semakin besar semakin baik


# Dengan Reguralized ------------------------------------------------------

# alpha: the elasticnet mixing parameter. Allowed values include:
#   "1": for lasso regression -> yang tidak terlalu berpengaruh coefnya  0
#   "0": for ridge regression -> semua var masuk tapi yang tidak terlalu berpengaruh coefnya mendekati 0
#   a value between 0 and 1 (say 0.3) for elastic net regression.
# lamba: a numeric value defining the amount of shrinkage. Should be specify by analyst.
# lambda yang baik minimize the cross-validation prediction error rate
# didapat dari cv.glmnet

x <- as.matrix(train[,-9])
y <- train$diabetes

# Find the best lambda using cross-validation
set.seed(1) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Setting lambda = lambda.1se produces a simpler model
# compared to lambda.min, 
# but the model might be a little bit less accurate
# than the one obtained with lambda.min.


# Display regression coefficients
coef(model)

probabilities <- model %>% predict(as.matrix(xtest))
predicted.classes <- ifelse(probabilities > 0.65, 1, 0)
confusionMatrix(table(ytest, predicted.classes))



# Rare Case Logistic Regression -------------------------------------------
library(Zelig)
z.out <- zelig(diabetes~(.)^2, model = "relogit", tau = 268/500,
               case.control = "weighting",
               bias.correct = TRUE,
               data = train)
summary(z.out)


train_prob <- predict(z.out, train[,-9], type = 'response')
cmtrain <- table(train$diabetes, ifelse(train_prob[[1]] > 0.65, 1, 0))
confusionMatrix(cmtrain)

test_prob <- predict(z.out, xtest, type = 'response')
cmtest <- table(ytest, ifelse(test_prob[[1]] > 0.65, 1, 0))
confusionMatrix(cmtest)
