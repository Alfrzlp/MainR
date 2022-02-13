library(rewlr)
data(National_exam_id)
dim(National_exam_id)
# data$Species <- ifelse(data$Species == "setosa",0,1)
# Supposed that current sample data has 9 percent of rare event data, and the population has 2 percent of those rare event data.
(weight0 <- (1 - 0.09) / (1 - 0.02))
(weight1 <- (0.09) / (0.02))
iter <- 1000
tol <- 0.00001

fit <- rewlr(y ~ ., data = National_exam_id, weights0 = weight0, weights1 = weight1)
fit
summary(fit)
p <- predict(fit, newdata = National_exam_id)

pred <- ifelse(fit$fitted >= 0.65, 1, 0) %>% as.factor()
pred

m1 <- glm(y ~ ., data = National_exam_id, family = binomial)
pred1 <- ifelse(m1$fitted.values >= 0.65, 1, 0) %>% as.factor()
pred1

actual <- National_exam_id$y %>% as.factor()
actual


caret::confusionMatrix(actual, pred)
caret::confusionMatrix(actual, pred1)
MLmetrics::F1_Score(actual, pred1)
MLmetrics::F1_Score(actual, pred)

nnet::multinom(Species ~ ., data = iris)
glm(Species ~ ., iris %>%
  dplyr::filter(Species != "versicolor") %>%
  mutate(Species = as.factor(ifelse(Species == "setosa", 1, 0))),
family = binomial, control = glm.control(maxit = 100)
)
