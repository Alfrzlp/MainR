DecisionTree <- setRefClass(
  "DecisionTree",
  fields = list(
    formula = "formula",
    data = "data.frame",
    max_depth = "numeric",
    # min leaf size
    min_n = "numeric",
    minimize_func = "function",
    min_information_gain = "numeric"
  ),
  methods = list(
    initialize = function(...) {},
    fit = function(y, x_pred) {},
    predict = function(x_pred) {}
  )
)



# S3 ----------------------------------------------------------------------
summary.DecisionTree <- function(model) {
  data.tree::FromListSimple(.change_name(model))
}

print.DecisionTree <- function(model) {
  data.tree::FromListSimple(.change_name(model), nodeName = "Joseph I")
}

predict.DecisionTree <- function(model, newdata) {
  res <- modify(split(newdata, 1:nrow(newdata)), .f = .pred, result = model)
  unlist(res, use.names = F)
}


# helper ------------------------------------------------------------------
.change_name <- function(result) {
  idx_left <- which.max(names(result) == "left")
  idx_right <- which.max(names(result) == "right")

  if (length(idx_left) == 0) {
    return(result)
  } else {
    names(result)[names(result) == "right"] <- str_glue(' {result$variabel} {ifelse(result$is_numeric, "<", "=")} {result$split} {ifelse(result$right$pos == "leaf", paste("-- Predict :",result$right$pred), "")}')
    names(result)[names(result) == "left"] <- str_glue(' {result$variabel} {ifelse(result$is_numeric, "≥", "!=")} {result$split} {ifelse(result$left$pos == "leaf", paste("-- Predict :",result$left$pred), "")}')

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



# Test --------------------------------------------------------------------
predict(hasil, newdata = iris[, -5])
print(hasil)
summary(hasil)




apply(iris[, -5], 2, function(val) .pred(val, result = hasil))
