library(tidyverse)


# Hierarchical Clustering -------------------------------------------------
X <- iris %>% select(-Species)
X
dj <- dist(scale(X))
cc <- hclust(dj)
cc


#dendogram
plot(cc)



# Non Hierarchical Clustering ---------------------------------------------
obj <- c("A","B","C","D")
x1 <- c(5,-1,1,-3)
x2 <- c(3,1,-2,-2)
dat1 <- data.frame(obj, x1, x2, row.names = 1)

dat1



set.seed(123)
kMres<-kmeans(dat1, centers=2)
kMres$cluster
kMres$size
pairs(dat1,col=c(1:2)[kMres$cluster],pch=16)
