library(tidyverse)


# Hierarchical Clustering -------------------------------------------------
X <- iris %>% select(-Species)
X
dj <- dist(scale(X))
cc <- hclust(dj, method = "ave")
cc


# dendogram
plot(cc)



# Non Hierarchical Clustering ---------------------------------------------
obj <- c("A", "B", "C", "D")
x1 <- c(5, -1, 1, -3)
x2 <- c(3, 1, -2, -2)
dat1 <- data.frame(obj, x1, x2, row.names = 1)

dat1



set.seed(123)
kMres < -kmeans(dat1, centers = 2)
kMres$cluster
kMres$size
pairs(dat1, col = c(1:2)[kMres$cluster], pch = 16)



# -------------------------------------------------------------------------
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- USArrests %>%
  na.omit() %>%
  scale() %>%
  as.data.frame()

distance <- get_dist(df)
# viz distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

fviz_cluster(k2, data = df)
df %>%
  as_tibble() %>%
  mutate(
    cluster = k2$cluster,
    state = row.names(USArrests)
  ) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()



k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)





# elbow
set.seed(123)

# function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
  type = "b", pch = 19, frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total within-clusters sum of squares"
)

# cara cepat
fviz_nbclust(df, kmeans, method = "wss")



