random_forest <- function(frml, data, ntree = 500,
                          mtry = ifelse((!is.null(y) && !is.factor(y)),
                            max(floor((ncol(data) - 1) / 3), 1),
                            floor(sqrt(ncol(data) - 1))
                          ),
                          replace = TRUE,
                          sample_size = ifelse(replace, nrow(data), ceiling(.632 * nrow(data))),
                          max_depth = 3, min_samples_split = 20,
                          min_samples_leaf = 10, min_ig = 1e-3,
                          metric_func = "gini",
                          method = 'lm') {

  # -----
  dat <- model.frame(frml, data)
  frml <- as.formula(paste(rlang::f_lhs(frml), "~ ."))
  is_reg <- !(is.factor(dat[, 1]) | is.logical(dat[, 1]) | is.character(dat[, 1]))

  rf_res <- lapply(1:ntree, FUN = function(i) {
    dat_sub <- sample_n(dat, sample_size, replace = replace)

    res <- decision_tree(
      frml,
      data = dat_sub,
      metric_func = metric_func,
      max_depth = max_depth,
      min_samples_split = min_samples_split,
      min_samples_leaf = min_samples_leaf,
      min_ig = min_ig
    )
    preds <- predict(res, newdata = test, method = method)
    return(list(res = res, preds = preds))
  })

  predicted <- lapply(rf_res, function(x) x$preds) %>%
    do.call(cbind, .) %>%
    as.data.frame()

  if (is_reg) {
    # predicted <- rowMeans(predicted)
    predicted <- predicted %>%
      rowwise() %>%
      mutate(pred = mean(c_across(cols = everything()))) %>%
      pull(pred)
  } else {
    mode <- function(x) {
      names(which.max(table(x)))
    }
    predicted <- predicted %>%
      rowwise() %>%
      mutate(pred = mode(c_across(cols = everything()))) %>%
      pull(pred)
  }

  return(list(rf_res, predicted = predicted))
}




# klasifikasi -------------------------------------------------------------
iris <- iris %>% janitor::clean_names()

res_ <- random_forest(species ~ ., data = iris, min_samples_leaf = 15)
res_rf <- randomForest::randomForest(species ~ ., data = iris, nodesize = 15)         
res_ranger <- ranger::ranger(species ~ ., data = iris, min.node.size = 15)

table(iris$species, res_$predicted)
table(iris$species, res_rf$predicted)
table(iris$species, res_ranger$predictions)




# test --------------------------------------------------------------------
tooth <- ToothGrowth %>% janitor::clean_names()

res_ <- random_forest(len ~ ., data = tooth, min_samples_leaf = 1)
res_rf <- randomForest::randomForest(len ~ ., data = tooth)         
res_ranger <- ranger::ranger(len ~ ., data = tooth)

yardstick::rmse(tooth, len, res_$predicted)
yardstick::rmse(tooth, len, res_rf$predicted)
yardstick::rmse(tooth, len, res_ranger$predictions)



df_res <- lapply(res_[[1]], function(x) x$preds) %>%
  do.call(cbind, .) %>%
  as.data.frame()

df_res <- df_res %>% 
  bind_cols(len = tooth$len)

glimpse(df_res)
m1 <- lm(len ~ ., data = df_res, )
predict(m1, df_res)

