# Functiom ----------------------------------------------------------------
# Nilai entropy maksimal = keacakan maksimal
# misalnya pada sesuatu yg peluangnya sama spt melempar koin
# input : raw vector
entropy <- function(y) {
  p <- table(y) / length(y)
  -sum(p * log2(p + 1e-9))
}


# input : vector y
gini_impurity <- function(y){
  if(length(y) == 0) return(0)
  p <- table(y) / length(y)
  1 - sum(p^2)
}


# input : vector y
variance <- function(y){
  if(length(y) <= 1) return(0)
  var(y)
}


# information gain
# semakin tinggi information gain maka semakin baik var tersbut 
# untuk menjadi best predictor
information_gain <- function(y, x, func = gini_impurity){
  entp_tot <- func(y)
  s <- length(y)
  s1 <- length(y[x])
  s2 <- s - s1
  # return value ig
  if (s1 == 0 | s2 == 0) return(0)
  else return(entp_tot - s1/s * func(y[x]) - s2/s * func(y[!x]))
}


# Max Information Gain For A Single Feature
get_max_ig <- function(y, x, func = gini_impurity, min_n = 10){
  best_ig <- best_split <- previous_split <-  NA
  is_numeric <- !(is.factor(x)|is.logical(x)|is.character(x))
  
  for (val in sort(unique(x))) {
    # jika regresi
    if (is_numeric) mask <- x < val
    # jika klasifikasi
    else mask <- x == val
    
    ig <- information_gain(y, mask, func) 
    s1 <- sum(mask)
    s2 <- length(mask) - s1
    
    if(s1 >= min_n & s2 >= min_n){
      if(is.na(best_ig)){
        best_ig <- ig
        best_split <- ifelse(is.na(previous_split), val, mean(c(val, previous_split)))
      } else if(ig > best_ig){
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


# input : 
# - vector y
# - matrix or df from x predictor
get_feature_split <- function(y, x_pred, ...){
  res <- sapply(x_pred, function(x) get_max_ig(y, x, ...))
  idx <- which.max(res['best_ig', ])
  if(length(idx) == 0) return(res)
  else return(as.data.frame(c(res[, idx], var = names(idx))))
}

get_idsplit <- function(dat, res_ig){
  if(res_ig$is_numeric) idx <- dat[, res_ig$var] < res_ig$best_split
  else idx <- dat[, res_ig$var] == res_ig$best_split
  return(idx)
}



output <- list(depth = depth, pos = NULL, variabel = NULL, split = NULL, pred = NULL, is_numeric = NULL, dat = NULL, left = NULL, right = NULL)
# decision tree -----------------------------------------------------------
max_depth = 3
min_n = 10
min_ig = 1e-3
minimize_fun = gini_impurity

generate <- function(dat, depth = 0){
  y <- dat[, 1]
  x <- dat[, -1]
  is_regression <- identical(minimize_fun, variance)
  mode_y <- names(which.max(table(y)))
  output <- list(depth = depth)
  
  if(!is_regression & length(unique(y)) == 1){
    output$pos <- 'leaf'
    output$pred <- mode_y
    return(output)
  }
  
  if(depth <= max_depth){
    res <- get_feature_split(y, x, func = minimize_fun, min_n = min_n)
    if(all(is.na(res[1, ]))){
      output$pos <- 'leaf'
      output$pred <- ifelse(is_regression, mean(y), mode_y)
      return(output)
    } else {
      idx <- get_idsplit(dat, res)
      if(res$best_ig > min_ig){
        if(sum(idx) >= min_n & length(idx) - sum(idx) >= min_n){
          if(output$depth + 1 > max_depth){
            output$pos <- 'leaf'
            output$pred <- ifelse(is_regression, mean(y), mode_y)
            return(output)        
          } else {
            output$pos <- ifelse(depth == 0, 'root', 'decision')
            output$variabel <- res$var
            output$is_numeric <- res$is_numeric
            output$split <- res$best_split
            output$right <- generate(dat[idx, ], depth = output$depth + 1)
            output$left <- generate(dat[!idx, ], depth = output$depth + 1)
            return(output)
          }
        }
      } else {
        output$pos <- 'leaf'
        output$pred <- ifelse(is_regression, mean(y), mode_y)
        return(output)  
      }
    }
  }
}

# Predict -----------------------------------------------------------------
str(hasil)
x <- dat[, -1]
generate(dat) %>% str()
apply(x, 2, function(val) .pred(val, result = hasil))


.pred <- function(x, result){
  if(result$pos == "leaf"){
    return(result$pred)
  }else{
    if(result$is_numeric){
      if(x[, result$variabel] < result$split) .pred(x, result$right)
      else .pred(x, result$left)
    }else{
      if(x[, result$variabel] == result$split) .pred(x, result$right)
      else .pred(x, result$left)
    } 
  }
}


.pred(x[150, ], hasil)


predict1 <- function(x, hasil) {
  res <- modify(split(x, 1:nrow(x)), .f = .pred, result = hasil)
  unlist(res, use.names = F)
}

predict3 <- function(x, hasil) {
  res <- c()
  for (i in 1:nrow(x)) {
    res <- c(res, .pred(x[i, ], result = hasil))
  }
  res
}




# benchmark ---------------------------------------------------------------
library(microbenchmark)
pred_ben <- 
  microbenchmark(
    predict1(x, hasil),
    predict2(x, hasil),
    'looping' = predict3(x, hasil),
    'tree' = predict(m1, type = 'class'),
    'rpart' = predict(m2, type = 'class'),
    setup = set.seed(1)
  )

pred_ben
autoplot(pred_ben)
boxplot(pred_ben)




res1 <- predict1(x, hasil)
res2 <- predict2(x, hasil)
res3 <- predict3(x, hasil)
res_tree <- predict(m1, type = 'class')
res_rpart <- predict(m2, type = 'class')

all.equal(res1, res3)
all.equal(res1, res2)
all.equal(res2, res3)
all.equal(res1, as.character(res_tree))
all.equal(res1, as.character(res_rpart))



model_ben <- 
  microbenchmark(
    'tree' = tree::tree(species~., data = dat, minsize = 40, mincut = 20, mindev = 1e-7),
    'rpart'= rpart::rpart(species ~ ., data = dat, maxdepth = 3, minbucket = 20, minsplit = 40, cp = 1e-9),
    'ridson' = decision_tree(formula = species ~ ., dat = dat),
    setup = set.seed(1)
  )
model_ben

autoplot(model_ben)
boxplot(model_ben)


# Test --------------------------------------------------------------------
# kanan = <
# data iris
iris <- iris %>% janitor::clean_names()
frml <- species ~ .
dat <- model.frame(frml, data = iris)


max_depth = 3
min_n = 20
min_ig = 1e-3
minimize_fun = gini_impurity

hasil <- generate(dat)
str(hasil)





# data tooth
tooth <- ToothGrowth %>% janitor::clean_names()
frml2 <- len ~ .
dat2 <- model.frame(frml2, data = tooth)

max_depth = 5
min_n = 5
min_ig = 1e-3
minimize_fun <- variance
to_print <- list()

hasil2 <- generate(dat2)
str(hasil2)

to_print <- rev(to_print)
str(to_print)
str(rev(to_print))


# Package -----------------------------------------------------------------
m1 <- tree::tree(species~., data = dat, minsize = 40, mincut = 20, mindev = 1e-7)
m2 <- rpart::rpart(species ~ ., data = dat, maxdepth = 3, minbucket = 20, minsplit = 40, cp = 1e-9)
predict(m1, type = 'class')
predict(m2, type = 'class')


