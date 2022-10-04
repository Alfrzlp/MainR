library(microbenchmark)
library(tidyverse)
library(Rcpp)


sourceCpp('C:/MainCpp/r/classification.cpp')
sourceCpp('C:/MainCpp/r/regression.cpp')
# sourceCpp('C:/MainCpp/r/test.cpp')
#

# Data --------------------------------------------------------------------
# data 100.000
dim(storms)

set.seed(1)
ytest <- sample(storms$category, 100000, replace = T)

set.seed(1)
wind <- sample(storms$wind, 100000, replace = T)
xtest <- wind < 62.5

set.seed(1)
ystr <- sample(storms$status, 100000, replace = T)


# Unique ------------------------------------------------------------------
res <- microbenchmark(
  getUnique(xtest),
  table(xtest),
  times = 100
)
print(res, order = 'median')
autoplot(res)


res <- microbenchmark(
  getUniqueStr(ystr),
  table(ystr),
  times = 100
)
print(res, order = 'median')
autoplot(res)


# Numeric pakai getUnique
# string pakai table


# Gini --------------------------------------------------------------------
res <- microbenchmark(
  cpp_factor = gini_impurity(ytest),
  r_factor = .gini_impurity(ytest),
  
  cpp_str = gini_impurity(as.character(ytest)),
  r_str = .gini_impurity(as.character(ytest)),
  
  cpp_num = gini_impurity(as.numeric(ytest)),
  r_num = .gini_impurity(as.numeric(ytest)),
  
  times = 100
)
print(res, order = 'median')
autoplot(res)


sprintf("%.10f", gini_impurity(y))
sprintf("%.10f", .gini_impurity(y))


gini_impurity(as.numeric(y))
gini_impurity(as.complex(y))
gini_impurity(as.character(y))
gini_impurity(as.logical(x))
.gini_impurity(x)


# Information gini --------------------------------------------------------
res <- microbenchmark(
  cpp = information_gain(as.numeric(ytest), xtest),
  r = .information_gain(ytest, xtest, .gini_impurity),
  times = 500
)
print(res, order = 'median')
autoplot(res)



# Get Best Split ----------------------------------------------------------
res <- microbenchmark(
  cpp = getBestSplit(ytest, wind, 20),
  r = .get_best_ig(ytest, wind, .gini_impurity, 20),
  times = 100
)

print(res, order = 'median')
autoplot(res)


getBestSplit(y, x, 20)
getBestSplit(y, x, 20)
getBestSplit_num(y, x, 20)
.get_best_ig(y, x, .gini_impurity, 20)$best_split



# Get Feature Split -------------------------------------------------------
xpred <- data.frame(a = wind, b = wind / 100, c = wind^3/10)
head(xpred)

res <- microbenchmark(
  cpp = getFtSplit(ytest, xpred, 20),
  r = .get_feature_split(ytest, xpred, .gini_impurity, 20),
  times = 100
)

print(res, order = 'median')
autoplot(res)





getBestSplit(ytest, xpred$a, 20)
.get_best_ig(ytest, xpred$a, .gini_impurity, 20)

getBestSplit(ytest, xpred$b, 20)
.get_best_ig(ytest, xpred$b, .gini_impurity, 20)

getBestSplit(ytest, xpred$c, 20)
.get_best_ig(ytest, xpred$c, .gini_impurity, 20)

getFtSplit(y, iris[-5], 20)

# -------------------------------------------------------------------------
y <- iris$species
min_samples_leaf = 20
min_samples_split = 40
min_ig = 0
max_depth = 3



# -------------------------------------------------------------------------
res <- microbenchmark(
  cpp = generateNode(
    y, iris[-5], 0, max_depth, min_samples_leaf, min_samples_split, min_ig
  ),
  r = decision_tree(
    species ~ .,
    data = iris,
    metric_fun = "gini",
    max_depth = 3,
    min_samples_leaf = 20,
    min_ig = 1e-3
  ),
  tree = tree::tree(species~., data = iris, minsize = 40, mincut = 20, mindev = 1e-7),
  rpart = rpart::rpart(species ~ ., data = iris, maxdepth = 3, minbucket = 20, minsplit = 40, cp = 1e-9),
  times = 100
)

print(res, order = 'median')
autoplot(res)



# -------------------------------------------------------------------------
set.seed(1)
ytest <- sample(storms$category, 500000, replace = T)
set.seed(1)
a <- sample(storms$wind, 500000, replace = T)
set.seed(1)
b <- sample(storms$hour, 500000, replace = T)
set.seed(1)
c <- sample(storms$day, 500000, replace = T)


dftest <- data.frame(ytest, a, b, c)

res <- microbenchmark(
  cpp = generateNode(
    ytest, dftest[-1], 0, max_depth, min_samples_leaf, min_samples_split, min_ig
  ),
  r = decision_tree(
    ytest ~ .,
    data = dftest,
    metric_fun = "gini",
    max_depth = 3,
    min_samples_leaf = 20,
    min_ig = 1e-3
  ),
  tree = tree::tree(ytest~., data = dftest, minsize = 40, mincut = 20, mindev = 1e-7),
  rpart = rpart::rpart(ytest ~ ., data = dftest, maxdepth = 3, minbucket = 20, minsplit = 40, cp = 1e-9),
  times = 10
)

print(res, order = 'median')
autoplot(res)
