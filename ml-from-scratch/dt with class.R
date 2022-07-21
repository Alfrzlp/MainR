NodeTree <- setRefClass(
  "Node",
  fields = list(
    formula = "formula",
    data = "data.frame",
    max_depth = "numeric",
    # min leaf size
    min_samples_leaf = "numeric",
    min_ig = "numeric",
    metric_func = "function",
  ),
  methods = list(
    initialize = function(...) {},
    fit = function(y, x_pred) {},
    predict = function(x_pred) {}
  )
)

# ID3 
# cuma kategorik - pakai entrophy
# C4.5 
# bisa variabel numerik - pakai entropyh juga gini ratio
# bisa handele missing value

# deprecated --------------------------------------------------------------
.get_max_ig <- function(y, x, metric_func, min_samples_leaf) {
  best_ig <- best_split <- previous_split <- NA
  is_numeric <- !(is.factor(x) | is.logical(x) | is.character(x))
  
  for (val in sort(unique(x))) {
    # jika regresi
    if (is_numeric) {
      mask <- x < val
    } # jika klasifikasi
    else {
      mask <- x == val
    }
    
    ig <- information_gain(y, mask, metric_func)
    s1 <- sum(mask)
    s2 <- length(mask) - s1
    
    if (s1 >= min_samples_leaf & s2 >= min_samples_leaf) {
      if (is.na(best_ig)) {
        best_ig <- ig
        best_split <- ifelse(is.na(previous_split), val, mean(c(val, previous_split)))
      } else if (ig > best_ig) {
        best_ig <- ig
        best_split <- ifelse(is_numeric, mean(c(val, previous_split)), val)
      }
    }
    previous_split <- val
  }
  
  return(
    list(
      "best_ig" = best_ig,
      "best_split" = best_split,
      "is_numeric" = is_numeric
    )
  )
}



# function ----------------------------------------------------------------
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
  var(y)
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

.get_feature_split <- function(y, x_pred, func, min_samples_leaf) {
  if(is.data.frame(y)) y <- dplyr::pull(y)
  res <- sapply(x_pred, function(x) .get_best_ig(y, x, metric_func = func, min_samples_leaf = min_samples_leaf))
  idx <- which.max(res["best_ig", ])
  if (length(idx) == 0) {
    return(res)
  } else {
    return(as.data.frame(c(res[, idx], var = names(idx))))
  }
}

.get_idsplit <- function(dat, res_ig) {
  if (res_ig$is_numeric) {
    idx <- dat[, res_ig$var] < res_ig$best_split
  } else {
    idx <- dat[, res_ig$var] == res_ig$best_split
  }
  return(idx)
}

generate_node <- function(dat, depth = 0, max_depth, min_samples_leaf, min_ig, metric_func, is_regression) {
  y <- dat[, 1]
  x <- dat[, -1]
  mode_y <- names(which.max(table(y)))
  
  if (!is_regression & length(unique(y)) == 1) {
    return(list(
      depth = depth,
      pos = "leaf",
      pred = mode_y
    ))
  }
  
  if (depth <= max_depth) {
    res <- .get_feature_split(y, x, func = metric_func, min_samples_leaf = min_samples_leaf)
    if (all(is.na(res[1, ]))) {
      return(list(
        depth = depth,
        pos = "leaf",
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
              pred = ifelse(is_regression, mean(y), mode_y)
            ))
          } else {
            n_t <- nrow(dat)
            return(list(
              depth = depth,
              pos = ifelse(depth == 0, "root", "decision"),
              variabel = res$var,
              is_numeric = res$is_numeric,
              split = res$best_split,
              ig = res$best_ig,
              right = generate_node(dat[idx, ], depth = depth + 1, max_depth = max_depth, min_samples_leaf = min_samples_leaf, min_ig = min_ig, metric_func = metric_func, is_regression = is_regression),
              left = generate_node(dat[!idx, ], depth = depth + 1, max_depth = max_depth, min_samples_leaf = min_samples_leaf, min_ig = min_ig, metric_func = metric_func, is_regression = is_regression)
              # fi = (n_t/N) * (res$best_ig - nrow(dat[idx, ])/n_t * 0.444 - nrow(dat[!idx, ])/n_t * 0)
            ))
          }
        }
      } else {
        return(list(
          depth = depth,
          pos = "leaf",
          pred = ifelse(is_regression, mean(y), mode_y)
        ))
        # ----
      }
    }
  }
}

decision_tree <- function(formula, data, max_depth = 3, min_samples_leaf = 20, min_ig = 1e-3, metric_func = "gini impurity") {
  dat <- model.frame(formula, data)
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

#
# S3 ----------------------------------------------------------------------
summary.DecisionTree <- function(model) {
  data.tree::FromListSimple(.change_name(model))
}

print.DecisionTree <- function(model) {
  # cli::cli_div(theme = list(span.emph = list(color = "orange")))
  data.tree::FromListSimple(.change_name(model), nodeName = "root ")
  # cli::cli_end()
}

predict.DecisionTree <- function(model, newdata, handle_na = F) {
  if(handle_na) pred_fun <- .pred2
  else pred_fun <- .pred
  
  res <- modify(split(newdata, 1:nrow(newdata)), .f = pred_fun, result = model)
  unlist(res, use.names = F)
}


# helper ------------------------------------------------------------------
.change_name <- function(result) {
  idx_left <- which.max(names(result) == "left")
  idx_right <- which.max(names(result) == "right")

  if (length(idx_left) == 0) {
    return(result)
  } else {
    # names(result)[names(result) == "right"] <- cli::cli_text(' {result$variabel} {ifelse(result$is_numeric, "<", "=")} {.emph {result$split}} {ifelse(result$right$pos == "leaf", paste("-- Predict :", result$right$pred), "")}')
    # names(result)[names(result) == "left"] <- cli::cli_text(' {result$variabel} {ifelse(result$is_numeric, "≥", "!=")} {.emph {result$split}} {ifelse(result$left$pos == "leaf", paste("-- Predict :", result$left$pred), "")}')
    names(result)[names(result) == "right"] <- str_glue('{result$variabel} {ifelse(result$is_numeric, "<", "=")} {result$split} {ifelse(result$right$pos == "leaf", paste("-- Predict :",result$right$pred), "")}')
    names(result)[names(result) == "left"] <- str_glue('{result$variabel} {ifelse(result$is_numeric, "≥", "!=")} {result$split} {ifelse(result$left$pos == "leaf", paste("-- Predict :",result$left$pred), "")}')
    
    result[[idx_left]] <- .change_name(result[[idx_left]])
    result[[idx_right]] <- .change_name(result[[idx_right]])
    return(result)
  }
}

.pred <- function(x, result) {
  if (result$pos == "leaf") {
    return(result$pred)
  } else {
    if (result$is_numeric) {
    # dibagian sini untuk prediksi jika NA
      if (x[, result$variabel] < result$split) {
        .pred(x, result$right)
      } else {
        .pred(x, result$left)
      }
    } else {
      if (x[, result$variabel] == result$split) {
        .pred(x, result$right)
      } else {
        .pred(x, result$left)
      }
    }
  }
}

.pred2 <- function(x, result) {
  if (result$pos == "leaf") {
    return(result$pred)
  } else {
    if (result$is_numeric) {
      # dibagian sini untuk prediksi jika NA
      if(is.na(x[, result$variabel])){
        if(result$prob[1] > result$prob[2]) .pred2(x, result$right)
        else .pred2(x, result$left)
      } else if (x[, result$variabel] < result$split) {
        .pred2(x, result$right)
      } else {
        .pred2(x, result$left)
      }
    } else {
      if(is.na(x[, result$variabel])){
        if(result$prob[1] > result$prob[2]) .pred2(x, result$right)
        else .pred2(x, result$left)
      } else if (x[, result$variabel] == result$split) {
        .pred2(x, result$right)
      } else {
        .pred2(x, result$left)
      }
    }
  }
}


# Test --------------------------------------------------------------------
# iris
iris <- iris %>% janitor::clean_names()
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


# tooth
tooth <- ToothGrowth %>% janitor::clean_names()
hasil2 <- decision_tree(
  len ~ .,
  data = tooth,
  metric_func = "gini im",
  max_depth = 5,
  min_samples_leaf = 5
)
print(hasil2)
predict(hasil2, newdata = tooth[, -1])
summary(hasil)


# cdc
hasil3 <- decision_tree(
  survived ~ pclass + sex + age + sib_sp + parch + fare + embarked + gelar,
  data = df_train %>% drop_na(),
  max_depth = 15,
  min_samples_leaf = 15
)
print(hasil3) 
preds <- predict(hasil3, df_test %>% drop_na())
preds <- predict(hasil3, df_test, handle_na = T)

cm <- table(drop_na(df_test)$Survive, preds)
cm <- table(df_test$Survive, preds)
sum(diag(cm))/sum(cm)

# package -----------------------------------------------------------------
m1 <- tree::tree(
  factor(buy) ~ .,
  data = df_train %>% select(-id), 
  minsize = 40,
  mincut = 20,
  mindev = 1e-7
)

m1 <- rpart::rpart(
  survived ~ pclass + sex + age + sib_sp + parch + fare + embarked + gelar,
  data = df_train, 
  maxdepth = 15, 
  minbucket = 15, 
  minsplit = 20,
  cp = 1e-9
)
m1
m1$variable.importance
res <- sapply(iris[, -5], function(x) .get_best_ig(iris$species, x, metric_func = .gini_impurity, min_samples_leaf = 20))


m1tree <- tree::tree(
  survived ~ pclass + sex + age + sib_sp + parch + fare + embarked + gelar,
  data = df_train, 
  minsize = 40,
  mincut = 15,
  mindev = 1e-7
)

pred_tree <- predict(m1tree, df_test, type = 'class')
pred_dt <- as.numeric(preds)
pred_rpart <- predict(m1, df_test, type = 'class')

sum(pred_dt != pred_tree)
pred_dt == pred_rpart
sum(pred_dt != pred_rpart)

# dt = 0.7636364
# tree = 0.630303
# rpart = 0.7727273

cm <- table(drop_na(df_test)$Survive, pred_rpart)
cm <- table(df_test$Survive, pred_rpart)
cm <- table(df_test$Survive, pred_tree)
sum(diag(cm))/sum(cm)

# Benchmark ---------------------------------------------------------------
library(microbenchmark)
big_ben <- microbenchmark(
  ridson = decision_tree(
    factor(buy) ~ .,
    data = df_train %>% select(-id),
    max_depth = 10,
    min_samples_leaf = 20
  ),
  tree = tree::tree(
    factor(buy) ~ .,
    data = df_train %>% select(-id),
    minsize = 40, mincut = 20, mindev = 1e-7
  ),
  rpart = rpart::rpart(
    factor(buy) ~ .,
    data = df_train %>% select(-id), 
    maxdepth = 3, minbucket = 20,
    minsplit = 40, cp = 1e-9
  ),
  setup = set.seed(1)
)

big_ben
autoplot(big_ben)






dt_ben <- microbenchmark(
  clasf = decision_tree(
    species ~ .,
    data = iris,
    metric_fun = "gini impurity",
    max_depth = 3,
    min_samples_leaf = 20,
    min_ig = 1e-3
  ),
  # reg = decision_tree(
  #   len ~ .,
  #   data = tooth,
  #   metric_fun = "variance",
  #   max_depth = 5,
  #   min_samples_leaf = 5,
  #   min_ig = 1e-3
  # ),
  tree = tree::tree(species~., data = iris, minsize = 40, mincut = 20, mindev = 1e-7),
  rpart = rpart::rpart(species ~ ., data = iris, maxdepth = 3, minbucket = 20, minsplit = 40, cp = 1e-9),
  times = 150,
  setup = set.seed(1)
)
dt_ben
autoplot(dt_ben)


# lainnya -----------------------------------------------------------------
clean <- function() {
  cli_progress_bar("Cleaning data", total = 100)
  for (i in 1:100) {
    Sys.sleep(5/100)
    cli_progress_update()
  }
}
clean()
