library(MASS)

X <- matrix(c(9, 1, 5, 3, 1, 2), nrow = 3, byrow = T)
X <- matrix(c(3, 4, 6, -2, 3, 1), nrow = 3, byrow = T)
X

library(ggrepel)
par(mar = c(0, 0, 0, 0))

data.frame(X) %>% 
  ggplot() +
  geom_point(
    aes(x = X1, y = X2),
    inherit.aes = F
  ) +
  geom_point(
    aes(x = mean(X1), y = mean(X2)),
    inherit.aes = F,
    col = 'red', shape = 1, size = 3.5
  ) +
  geom_text_repel(
    aes(x = mean(X1), y = mean(X2)),
    label = 'mean'
  ) +
  theme_minimal()

library(plotly)
plot_ly(x = X[1,], y = X[2,], z = X[3,],
        type="scatter3d", mode="lines+markers")




d1 <- X[,1] - mean(X[,1])
d2 <- X[,2] - mean(X[,2])
d1
d2
len(d1)
len(d2)

# Matrix sample covariance
angle(d1, d2)

n <- length(d1)
# Sn
Sn <- matrix(c(len(d1)^2/n, sum(d1*d2)/n,
         sum(d1*d2)/n, len(d2)^2/n), 2, byrow = T)
Sn %>% fractions()
# Matrix korelasi (R)

R <- `diag<-`(Sn / (sqrt(Sn[1, 1]*Sn[2, 2])), 1)
R
R %>% fractions()






# 3.3 ---------------------------------------------------------------------
y1 <- c(1, 4, 4)
x1 <- 3*c(1, 1, 1)

y1 - x1
x1 + (y1 - x1)


# 3.5 ---------------------------------------------------------------------
# generalized sample variance |S|
(X <- matrix(c(9, 1, 5, 3, 1, 2), nrow = 3, byrow = T))
cov(X)
det(cov(X))

(X <- matrix(c(3, 4, 6, -2, 3, 1), nrow = 3, byrow = T))
cov(X)
det(cov(X))


X - matrix(colMeans(X), nrow  = nrow(X), ncol = ncol(X), byrow = T)
colMeans(X)


# 3.6 ---------------------------------------------------------------------
(X <- matrix(c(-1, 3, -2, 2, 4, 2, 5, 2, 3), nrow = 3, byrow = T))

# a. matrix deviasi (residual), X-1xbar apakah full rank
colMeans(X)
d <- X - matrix(colMeans(X), nrow  = nrow(X), ncol = ncol(X), byrow = T)
d

# full rank max 3 untuk matrix 3x3
# jika ada kombinasi 1 maka rank = 2
# tidak ada kombinasi maka full rank


# b. Hitung S dan generalized sample variance |S|
cov(X)
det(cov(X))
# 0


# secara geometry volume yang terbentuk dari vector1 deviasinya
# adalah 0. d3 merupakan kombinasi liniear dari d1 dan d2. sehingga
# apabila diilustrasikan d3 berada pada bidang datar d1 dan d2
# sehingga tidak ada tinggi hanya ada alas maka volume 0


library(plotly)
plot_ly(
  x = d[1,], y = d[2,], z = d[3,],
  type = "scatter3d",
  mode = "lines+markers"
)



# c. Total sample variance
# Sigma Sii
sum(diag(cov(X)))


# 3.7 ---------------------------------------------------------------------


