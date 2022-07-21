# coba pakai rumus var populasi

generate_node <- function(dat, depth = 0, max_depth, min_samples_leaf, min_ig, metric_func, is_regression) {
  y <- dat[, 1]
  x <- dat[, -1]
  mode_y <- names(which.max(table(y)))
  res <- .get_feature_split(y, x, func = metric_func, min_samples_leaf = min_samples_leaf)
  
  if (!is_regression & length(unique(y)) == 1) {
    return(list(
      depth = depth,
      pos = "leaf",
      n = nrow(dat),
      ig = ifelse(is.null(res$best_ig), 0, res$best_ig),
      pred = mode_y
    ))
  }
  
  if (depth <= max_depth) {
    res <- .get_feature_split(y, x, func = metric_func, min_samples_leaf = min_samples_leaf)
    if (all(is.na(res[1, ]))) {
      return(list(
        depth = depth,
        pos = "leaf",
        n = nrow(dat),
        ig = ifelse(is.null(res$best_ig), 0, res$best_ig),
        pred = ifelse(is_regression, mean(y), mode_y)
      ))
    } else {
      idx <- .get_idsplit(dat, res)
      if (res$best_ig > min_ig) {
        if (sum(idx) >= min_samples_leaf & length(idx) - sum(idx) >= min_samples_leaf) {
          if (depth + 1 > max_depth) {
            return(list(
              depth = depth,
              pos = "leaf",
              n = nrow(dat),
              ig = ifelse(is.null(res$best_ig), 0, res$best_ig),
              pred = ifelse(is_regression, mean(y), mode_y)
            ))
          } else {
            n_t <- nrow(dat)
            output <- list(
              depth = depth,
              pos = ifelse(depth == 0, "root", "decision"),
              variabel = res$var,
              is_numeric = res$is_numeric,
              split = res$best_split,
              n = n_t,
              ig = res$best_ig,
              right = generate_node(dat[idx, ], depth = depth + 1, max_depth = max_depth, min_samples_leaf = min_samples_leaf, min_ig = min_ig, metric_func = metric_func, is_regression = is_regression),
              left = generate_node(dat[!idx, ], depth = depth + 1, max_depth = max_depth, min_samples_leaf = min_samples_leaf, min_ig = min_ig, metric_func = metric_func, is_regression = is_regression)
            )
            
            fi <- (n_t/N) * (res$best_ig - nrow(dat[idx, ])/n_t * output$right$ig - nrow(dat[!idx, ])/n_t * output$left$ig)
            df_fi[df_fi$name == res$var, 2] <<- df_fi[df_fi$name == res$var, 2] + fi
            return(output)
          }
        }
      } else {
        return(list(
          depth = depth,
          pos = "leaf",
          n = nrow(dat),
          ig = ifelse(is.null(res$best_ig), 0, res$best_ig),
          pred = ifelse(is_regression, mean(y), mode_y)
        ))
        # ----
      }
    }
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
  
  df_fi <- data.frame(name = names(dat[, -1]), fi = 0)
  result <- generate_node(dat, depth = 0, max_depth = max_depth, min_samples_leaf = min_samples_leaf, min_ig = min_ig, metric_func = metric_func, is_regression = is_reg)
  class(result) <- "DecisionTree"
  return(list(res = result, feature_importance = df_fi))
}

# -------------------------------------------------------------------------
library(tidyverse)

iris <- iris %>% janitor::clean_names()
N <- nrow(iris)
hasil <- decision_tree(
  species ~ .,
  data = iris,
  metric_func = "gini impurity",
  max_depth = 3,
  min_samples_leaf = 20,
  min_ig = 1e-3
)
hasil
str(hasil)
print(hasil)
predict(hasil, newdata = iris[, -5])

df_fi
df_fi %>% 
  group_by(name) %>% 
  summarise_all(sum) %>% 
  mutate(norm_fi = fi/sum(fi))

# california housing
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
  min_ig = 1e-3
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

hasil_rpart <- rpart::rpart(species ~ ., data = iris, maxdepth = 3, minbucket = 20, minsplit = 40, cp = 1e-9)
hasil_rpart <- rpart::rpart(price ~ ., data = dat, maxdepth = 3, minbucket = 20, minsplit = 2, cp = 1e-9)
hasil_rpart$variable.importance
scale(hasil_rpart$variable.importance)

colnames(iris)
# 0.01339286 0.01339286 0.0398032  0.59938685
# normalisasi
# 0.02011013 0.02011013 0.05976673 0.90001301


hasil$ig
hasil$right$ig
hasil$left$ig


# -------------------------------------------------------------------------
iris <- iris %>% janitor::clean_names()
x_pred <- iris %>% select(-species)
y <- iris %>% select(species) %>% pull()
x <- x_pred$sepal_length

sapply(x_pred, function(x) .get_best_ig(y, x, metric_func = .gini_impurity, min_samples_leaf = 10))





library(tidyverse)
X <- c(1,0,0, 0,0,0, 0,0,1, 0,1,0)
X <- matrix(X, nrow = 4, ncol = 3, byrow = T)
y = c(1,0,1,1)

dat <- cbind(y, X)
dat <- dat %>% `colnames<-`(c('y', 'x1', 'x2', 'x3'))
dat <- as.data.frame(dat) 
dat %>% glimpse()

hasil <- decision_tree(factor(y) ~ ., dat, max_depth = 3, min_samples_leaf = 1)
str(hasil)
print(hasil)
predict(hasil, dat)

1 * (0.0417 - (3/4)*0.111 - 0)
0.75 * (0.111 - (2/3)*0.5 - 0)

fi <- c(-0.04155, -0.16675, 0.25)
fi
scale(fi)


m1 <- rart::rpart(factor(y) ~ ., dat, control = rpart::rpart.control(minbucket = 1, minsplit = 2)) 
m1
m1$variable.importance
# 1.0000000 0.3333333 0.1666667 
predict(m1, type = 'class')
y
# -------------------------------------------------------------------------
library(ggplot2)
library(magick)
img <- image_read('E:/wallpaper/pexels-aarti-vijay-2693529.jpg')
ggplot(mapping = aes(1:10, 1:10)) +
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(
    x = NULL, y = NULL,
    title = 'gambar saya'
  ) +
  theme(
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
