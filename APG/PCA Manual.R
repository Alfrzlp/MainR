print_eq <- function(eig_vec, comp = 1:ncol(eig_vec)){
  for (i in comp) {
    cat(stringr::str_glue('y{i} = {paste0(round(eig_vec[,i], 3), "*X", 1:length(eig_vec[,i]), collapse = " + ")}'), '\n')
  }
}
scree_plot <- function(eig_values){
  ggplot(NULL, aes(y = eig_values, x = 1:length(eig_values))) +
    geom_line() +
    geom_point(color = 'red') +
    geom_text(aes(label = round(eig_values, 3)), nudge_x = 0.2, nudge_y = 0.2) +
    labs(
      x = 'Component Number',
      y = 'Eigen Value',
      title = 'Scree Plot'
    ) +
    theme_bw()
}


# Pendekatan matriks kovarians --------------------------------------------
S <- matrix(c(6, 3, 3, 4), 2, 2)
eig <- eigen(S)
eig

# proporsi
eig$values / sum(eig$values)

# persamaan ke 1
print_eq(eig$vectors, 1)

# score
eig$vectors
score <- as.matrix(X) %*% eig$vectors
score 





# Pendekatan Matrix Korelasi ----------------------------------------------
V12 <- diag(sqrt(diag(S)))
R <- solve(V12) %*% S  %*% solve(V12) 
R

eig.r <- eigen(R)
eig.r
eig.r$values / sum(eig.r$values)




# Membangkitkan data ------------------------------------------------------
miu <- c(2, 5)
X <- mvtnorm::rmvnorm(30, miu, S)
X


S <- cov(X)
S

eig <- eigen(S)
# proporsi
eig$values / sum(eig$values)


# y1
y1 <- eig$vectors[1, 1] * X[, 1] + eig$vectors[2, 1] * X[, 2]
y2 <- eig$vectors[1, 2] * X[, 1] + eig$vectors[2, 2] * X[, 2]

cbind(y1, y2)
cbind(X, y1, y2)






# distandarisasi
eig <- eigen(cov(scale(X)))
eig$values / sum(eig$values)
# pendekatan matrix korelasi
eig <- eigen(cor(X))
eig$values / sum(eig$values)




# Dengan Function ---------------------------------------------------------
X <- iris[, -5]

pc <- princomp(X, cor = F)
summary(pc, loadings = T)
# loadings adalah print_eq
loadings(pc)
# std
sqrt(eig$values)

screeplot(pc, type = 'line', col = 4)
pc$scores


cor(X, pc$scores)






# Korelasi antara KU dengan Variabel --------------------------------------
# hanya untuk pendekatan matrix korelasi

S <- cov(X)
R <- cor(X)
# manual
eig <- eigen(R)
eig

# proporsi
eig$values / sum(eig$values)

print_eq(eig$vectors)

korelasi <- 
  sweep(
    eig$vectors, 2, sqrt(eig$values), 
    FUN = function(a, b) a*b
  ) %>% 
  `dimnames<-`(
    list(paste0('x', 1:nrow(.)), paste0('y', 1:ncol(.)))
  )
korelasi


pc <- princomp(X, cor = F)
loadings(pc)
cor(X, pc$scores)



