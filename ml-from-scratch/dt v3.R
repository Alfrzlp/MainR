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
    
    stringr::str_glue("{round(metric_func(y), 3)} - {s1}/{s} * {round(metric_func(y[x]), 3)} - {s2}/{s} * {round(metric_func(y[!x]), 3)}\n\n\n") %>% cat()
    return(metric_func(y) - s1 / s * metric_func(y[x]) - s2 / s * metric_func(y[!x]))
  }
}

# get best ig -------------------------------------------------------------
.get_best_ig <- function(y, x, metric_func, min_samples_leaf){
  is_numeric <- !(is.factor(x) | is.logical(x) | is.character(x))
  
  unique_x <- sort(unique(x))
  if(is_numeric){
    all_mask <- lapply(unique_x, function(val) x < val)
  }else{
    all_mask <- lapply(unique_x, function(val) x == val)
  }
  names(all_mask) <- unique_x
  
  all_mask <- purrr::keep(
    all_mask, function(x) sum(x) >= min_samples_leaf & (length(x) - sum(x)) >= min_samples_leaf)
  # tidak ada yang memenuhi min samples leaf
  if(length(all_mask) == 0){
    return(list(
      best_ig = NA,
      best_split = NA,
      is_numeric = is_numeric
    ))
  }else{
    all_ig <- lapply(all_mask, function(x) .information_gain(y, x, metric_func))
    all_ig <- unlist(all_ig)
    print(all_ig)
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


# Get feature split -------------------------------------------------------
.get_feature_split <- function(y, x_pred, func, min_samples_leaf) {
  res <- sapply(x_pred, function(x) .get_best_ig(y, x, metric_func = func, min_samples_leaf = min_samples_leaf))
  idx <- which.max(res["best_ig", ])
  if (length(idx) == 0) return(res)
  else {
    return(as.data.frame(c(res[, idx], var = names(idx))))
  }
}

# Compute fi --------------------------------------------------------------
compute_fi <- function(N, n_t, idx, y, func){
  fi <- (n_t/N) * (func(y) - length(y[idx])/n_t * func(y[idx]) - length(y[!idx])/n_t * func(y[!idx]))
  return(fi)
}


# Gnerate Node ------------------------------------------------------------
# klasifikasi
generate_node <- function(df_sub, depth = 0, max_depth, min_samples_leaf, min_samples_split, min_ig, metric_func) {
  y <- df_sub[,  1]
  x <- df_sub[, -1]
  mode_y <- names(which.max(table(y)))
  n_t <- nrow(df_sub)
  
  if (length(unique(y)) == 1) {
    return(list(
      depth = depth,
      pos = "leaf",
      pred = mode_y
    ))
  }
  
  res <- .get_feature_split(
    y, x, func = metric_func,
    min_samples_leaf = min_samples_leaf
  )
  # all isna artinya min sample leaf tidak memenuhi
  if((depth + 1 <= max_depth) && (res$best_ig >= min_ig) && (n_t >= min_samples_split) && !all(is.na(res[1, ]))){
    if (res$is_numeric) {
      idx <- df_sub[, res$var] < res$best_split
    } else {
      idx <- df_sub[, res$var] == res$best_split
    }
    

    output <- list(
      depth = depth,
      pos = ifelse(depth == 0, "root", "decision"),
      variabel = res$var,
      is_numeric = res$is_numeric,
      split = res$best_split,
      right = generate_node(
        df_sub[idx, ], depth = depth + 1,
        max_depth = max_depth,
        min_samples_leaf = min_samples_leaf,
        min_samples_split = min_samples_split,
        min_ig = min_ig, 
        metric_func = metric_func
      ),
      left = generate_node(
        df_sub[!idx, ], depth = depth + 1,
        max_depth = max_depth,
        min_samples_leaf = min_samples_leaf,
        min_samples_split = min_samples_split,
        min_ig = min_ig, 
        metric_func = metric_func
      )
    )
    return(output)
  }else{
    return(list(
      depth = depth,
      pos = "leaf",
      pred = mode_y
    ))
  }
  # ----
}

# regresi
generate_nodeReg <- function(df_sub, depth = 0, max_depth, min_samples_leaf, min_samples_split, min_ig, metric_func) {
  y <- df_sub[,  1]
  x <- df_sub[, -1]
  n_t <- nrow(df_sub)

  res <- .get_feature_split(
    y, x, func = metric_func,
    min_samples_leaf = min_samples_leaf
  )
  # all isna artinya min sample leaf tidak memenuhi
  if((depth + 1 <= max_depth) && (res$best_ig >= min_ig) && (n_t >= min_samples_split) && !all(is.na(res[1, ]))){
    
    if (res$is_numeric) {
      idx <- df_sub[, res$var] < res$best_split
    } else {
      idx <- df_sub[, res$var] == res$best_split
    }
    
    fi <- compute_fi(N, n_t, idx, df_sub[, 1], metric_func)
    df_fi[df_fi$name == res$var, 2] <<- df_fi[df_fi$name == res$var, 2] + fi
    
    output <- list(
      depth = depth,
      pos = ifelse(depth == 0, "root", "decision"),
      variabel = res$var,
      is_numeric = res$is_numeric,
      split = res$best_split,
      right = generate_nodeReg(
        df_sub[idx, ], depth = depth + 1,
        max_depth = max_depth,
        min_samples_leaf = min_samples_leaf,
        min_samples_split = min_samples_split,
        min_ig = min_ig, 
        metric_func = metric_func
      ),
      left = generate_nodeReg(
        df_sub[!idx, ], depth = depth + 1,
        max_depth = max_depth,
        min_samples_leaf = min_samples_leaf,
        min_samples_split = min_samples_split,
        min_ig = min_ig, 
        metric_func = metric_func
      )
    )
    return(output)
  }else{
    return(list(
      depth = depth,
      pos = "leaf",
      pred = mean(y)
    ))
  }
  # ----
}




# decision tree -----------------------------------------------------------
decision_tree <- function(
    formula, data, max_depth = 3, min_samples_leaf = 20,
    min_samples_split = min_samples_leaf * 2, min_ig = 1e-3,
    metric_func = "gini") {
  
  dat <- model.frame(formula, data)
  metric_func <- match.arg(metric_func, c('gini impurity', 'variance', 'entropy'))
  is_reg <- !(is.factor(dat[, 1]) | is.logical(dat[, 1]) | is.character(dat[, 1]))
  N <- nrow(dat)
  df_fi <- data.frame(name = names(dat[, -1]), fi = 0)
  
  if (metric_func == 'gini impurity' & !is_reg) {
    metric_func <- .gini_impurity
    result <- generate_node(
      dat, depth = 0, 
      max_depth = max_depth,
      min_samples_leaf = min_samples_leaf,
      min_samples_split = min_samples_split,
      min_ig = min_ig, 
      metric_func = metric_func
    )
  } else if (metric_func == "variance" & is_reg) {
    metric_func <- .variance
    result <- generate_nodeReg(
      dat, depth = 0, 
      max_depth = max_depth,
      min_samples_leaf = min_samples_leaf,
      min_samples_split = min_samples_split,
      min_ig = min_ig, 
      metric_func = metric_func
    )
  } else if (metric_func == "entropy" & !is_reg){
    metric_func <- .entropy
    result <- generate_node(
      dat, depth = 0, 
      max_depth = max_depth,
      min_samples_leaf = min_samples_leaf,
      min_samples_split = min_samples_split,
      min_ig = min_ig, 
      metric_func = metric_func
    )
  }else {
    stop('metric function tidak sesuai')
  }
  
  class(result) <- "DecisionTree"
  return(result)
}




# test --------------------------------------------------------------------
library(tidyverse)

iris <- iris %>% janitor::clean_names()
N <- nrow(iris)
df_fi <- data.frame(name = names(iris[-1]), fi = 0)
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
df_fi$n_fi <- df_fi$fi / sum(df_fi$fi)
df_fi

# 2	petal length (cm)	0.567487
# 3	petal width (cm)	0.432513


# california housing ------------------------------------------------------
dat <- read.csv('D:/__Datasets/ml/califorania_housing.csv')
dat <- dat %>%
  select(-c('X', 'Latitude', 'Longitude', 'Population'))
head(dat)
dim(dat)

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


# Benchmark ---------------------------------------------------------------
# klasifikasi
dt_ben <- microbenchmark(
  clasf = decision_tree(
    species ~ .,
    data = iris,
    metric_fun = "gini",
    max_depth = 3,
    min_samples_leaf = 20,
    min_ig = 1e-3
  ),
  tree = tree::tree(species~., data = iris, minsize = 40, mincut = 20, mindev = 1e-7),
  rpart = rpart::rpart(species ~ ., data = iris, maxdepth = 3, minbucket = 20, minsplit = 40, cp = 1e-9),
  times = 150,
  setup = set.seed(1)
)

print(dt_ben, order = 'median')
autoplot(dt_ben)






