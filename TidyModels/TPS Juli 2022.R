library(tidyverse)
library(cluster) 
library(factoextra)
tidymodels::tidymodels_prefer()

# Data --------------------------------------------------------------------
dat <- read.csv('D:/__Datasets/ml/tabular-playground-series-jul-2022/data.csv')
df_sub <- read.csv('D:/__Datasets/ml/tabular-playground-series-jul-2022/sample_submission.csv')

glimpse(dat)
glimpse(df_sub)

X <- dat %>% select(-id)
rpca <- princomp(X)
summary(rpca)

loadings(rpca)
eigen(cov(X))
# 23

X <- rpca$scores[, 1:23]


# Kmeans ------------------------------------------------------------------
distance <- get_dist(sample(X, 50000), stand = T)
# viz distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(X, centers = 17, nstart = 1)
str(k2)

fviz_cluster(k2, data = X)
k2$cluster



# N optimum ---------------------------------------------------------------
wss <- function(k) {
  kmeans(X, k, nstart = 1)$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:20
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares"
)


k2 <- kmeans(X, centers = 7, nstart = 1, iter.max = 1000, algorithm = 'Forgy')
# Submission --------------------------------------------------------------
df_sub %>%
  mutate(Predicted = k2$cluster) %>% 
  write.csv('D:/__Datasets/sub.csv', row.names = F, quote = F)  

system(
  'kaggle competitions submit -c tabular-playground-series-jul-2022 -f D:/__Datasets/sub.csv -m "Message"'
)
