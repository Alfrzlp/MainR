library(tidyverse)

# criterion ---------------------------------------------------------------
.gini_impurity <- function(y) {
  if (length(y) == 0) {
    return(0)
  }
  p <- table(y) / length(y)
  1 - sum(p^2)
}

.variance <- function(y) {
  if (length(y) <= 1) {
    return(0)
  }
  # var(y)
  # python pakai ini
  var(y) * (length(y) - 1) / length(y)
}

.entropy <- function(y) {
  p <- table(y) / length(y)
  -sum(p * log2(p + 1e-9))
}

.information_gain <- function(y, x, metric_func) {
  s <- length(y)
  s1 <- length(y[x])
  s2 <- s - s1
  # return value ig
  if (s1 == 0 | s2 == 0) {
    return(0)
  } else {
    return(metric_func(y) - s1 / s * metric_func(y[x]) - s2 / s * metric_func(y[!x]))
  }
}

.get_best_ig <- function(y, x, metric_func, min_samples_leaf){
  is_numeric <- !(is.factor(x) | is.logical(x) | is.character(x))

  unique_x <- sort(unique(x))
  if(is_numeric){
    all_mask <- lapply(unique_x, function(val) x < val)
  }else{
    all_mask <- lapply(unique_x, function(val) x == val)
  }
  names(all_mask) <- unique_x
  all_mask <- keep(all_mask, function(x) sum(x) >= min_samples_leaf & (length(x) - sum(x)) >= min_samples_leaf)
  if(length(all_mask) == 0){
    return(list(
      best_ig = NA,
      best_split = NA,
      is_numeric = is_numeric
    ))
  }else{
    all_ig <- lapply(all_mask, function(x) .information_gain(y, x, metric_func))
    all_ig <- unlist(all_ig)
    best_ig <- all_ig[which.max(all_ig)]
    val <- names(best_ig)

    if(is_numeric){
      val <- as.numeric(val)
      best_split <- mean(c(val, max(unique_x[unique_x < val])))
    }else{
      best_split <- val
    }
    return(list(
      best_ig = best_ig[[1]],
      best_split = best_split,
      is_numeric = is_numeric
    ))
  }
}



decision_tree <- function(
    formula, data, max_depth = 3, min_samples_leaf = 20,
    min_samples_split = min_samples_leaf * 2, min_ig = 1e-3,
    metric_func = "gini impurity") {

  dat <- model.frame(formula, data)
  N <- nrow(dat)
  metric_func <- match.arg(metric_func, c('gini impurity', 'gini index', 'variance', 'entropy'))

  is_reg <- !(is.factor(dat[, 1]) | is.logical(dat[, 1]) | is.character(dat[, 1]))

  if (metric_func %in% c('gini impurity', 'gini index')) {
    metric_func <- .gini_impurity
  } else if (metric_func == "variance" & is_reg) {
    metric_func <- .variance
  } else if (metric_func == "entropy") metric_func <- .entropy
  else {
    stop('metric function tidak sesuai')
  }

  result <- generate_node(dat, depth = 0, max_depth = max_depth, min_samples_leaf = min_samples_leaf, min_ig = min_ig, metric_func = metric_func, is_regression = is_reg)
  class(result) <- "DecisionTree"
  return(result)
}


# Testing -----------------------------------------------------------------
library(tidyverse)

iris <- iris %>% janitor::clean_names()
N <- nrow(iris)
df_fi <- data.frame(name = names(iris[-5]), fi = 0)
df_fi

hasil <- decision_tree(
  species ~ .,
  data = iris,
  metric_func = "gini impurity",
  max_depth = 3,
  min_samples_leaf = 20,
  min_ig = 0
)
hasil
str(hasil)
print(hasil)


df_fi
df_fi %>%
  group_by(name) %>%
  summarise_all(sum) %>%
  mutate(norm_fi = fi/sum(fi))

# 2	petal length (cm)	0.567487
# 3	petal width (cm)	0.432513




# regresi iris ------------------------------------------------------------
N <- nrow(iris)
df_fi <- data.frame(name = names(iris[-1]), fi = 0)
df_fi

hasil <- decision_tree(
  sepal_length ~ .,
  data = iris,
  metric_func = "gini impu",
  max_depth = 3,
  min_samples_leaf = 20,
  min_ig = 0
)
hasil
str(hasil)
print(hasil)




df_fi
df_fi %>%
  group_by(name) %>%
  summarise_all(sum) %>%
  mutate(norm_fi = fi/sum(fi))

y_pred <- predict(hasil, iris)
yardstick::mae(iris, truth = sepal_length, y_pred)
yardstick::rmse(iris, truth = sepal_length, y_pred)



var(iris$sepal_length) * (150 - 1) / 150
p <- table(iris$species) / length(iris$species)
1 - sum(p^2)



# california housing ------------------------------------------------------
dat <- read.csv('D:/__Datasets/ml/califorania_housing.csv')
dat <- dat %>%
  select(-c('X', 'Latitude', 'Longitude', 'Population'))
head(dat)

df_fi <- data.frame(name = names(dat[-6]), fi = 0)
df_fi

hasil2 <- decision_tree(
  price ~ .,
  data = dat,
  metric_func = "variance",
  max_depth = 3,
  min_samples_leaf = 2,
  min_ig = 0
)

print(hasil2)
str(hasil2)
df_fi
df_fi %>%
  group_by(name) %>%
  summarise_all(sum) %>%
  mutate(norm_fi = fi/sum(fi))
# ‘MedInc’: 0.854,
# ‘HouseAge’: 0.0,
# ‘AveRooms’: 0.027,
# ‘AveBedrms’: 0.0,
# ‘AveOccup’: 0.12


y_pred <- predict(hasil2, dat)
yardstick::mae(dat, truth = price, y_pred)
yardstick::rmse(dat, truth = price, y_pred)
