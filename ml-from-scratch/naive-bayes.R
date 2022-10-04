
# data --------------------------------------------------------------------
s <- 'No Outlook Temperature Humidity Windy Play
1 Sunny Hot High False No
2 Sunny Hot High True No
3 Cloudy Hot High False Yes
4 Rainy Mild High False Yes
5 Rainy Cool Normal False Yes
6 Rainy Cool Normal True Yes
7 Cloudy Cool Normal True Yes
8 Sunny Mild High False No
9 Sunny Cool Normal False Yes
10 Rainy Mild Normal False Yes
11 Sunny Mild Normal True Yes
12 Cloudy Mild High True Yes
13 Cloudy Hot Normal False Yes
14 Rainy Mild High True No'

dat <- dm::read_string(s, header = T) %>%
  janitor::clean_names() %>% 
  select(-1)



# Naive Bayes -------------------------------------------------------------
library(e1071)

set.seed(1)
nb <- naiveBayes(play ~ ., data = dat)
nb
table(y, predict(nb, dat))



y <- dat$play
n <- length(y)

# Prior Probability of Classes P(y)
prior <- table(y)/n

# Calculate the Likelihood Table for all features
dat

lld <- lapply(dat, function(x) table(y, x) / n)
lld

# Calculate Posterior Probability for each class using the Naive Bayesian equation.
# The Class with maximum probability is the outcome of the prediction.

dat
tbl <- lld[colnames(dat)[1]][[1]]
tbl["Yes", "Sunny"]


col_name <- colnames(dat)
col_name <- col_name[col_name != "play"]
col_name

dat[, col_name[1]]
tbl <- lld[col_name[1]][[1]]
tbl["Yes", dat[, col_name[1]]]


lapply(1:ncol(dat), function(idx){
  tbl <- lld[idx][[1]]
  tbl["Yes", dat[, idx]]
}) 



res <- lapply(1:(ncol(dat) - 1), function(idx){
  tbl <- lld[idx][[1]]
  tbl["Yes", dat[, idx]]
}) 

res <- do.call("cbind", res)
res <- cbind(res, prior["Yes"])
rownames(res) <- NULL
res

yes <- apply(res, 1, prod)
yes



res <- lapply(1:(ncol(dat) - 1), function(idx){
  tbl <- lld[idx][[1]]
  tbl["No", dat[, idx]]
}) 

res <- do.call("cbind", res)
res <- cbind(res, prior["No"])
rownames(res) <- NULL
res

no <- apply(res, 1, prod)
no


pred_yes <- yes / (yes + no)
pred_no <- 1 - pred_yes

pred <- ifelse(pred_yes >= pred_no, "Yes", "No")
table(y, pred)




dat
naivebayes::naive_bayes(y = dat$play, x = as.matrix(dat[-5]))
