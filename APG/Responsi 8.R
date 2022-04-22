library(tidyverse)

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

# -------------------------------------------------------------------------
R <- matrix(c(1, 0.9, 0.74, 0.64,
              0.9, 1, 0.33, 0.74,
              0.74, 0.33, 1, 0.66,
              0.64, 0.74, 0.66, 1),
            nrow = 4, byrow = T)
R


# A -----------------------------------------------------------------------
eig <- eigen(R)
eig

# proporsi
eig$values / sum(eig$values)

# scree plot
scree_plot(eig$values)

# maka komponen utama yang sebaiknya digunakan adalah 1
# karena sudah dapat menjelaskan 75,47% variasi. Selain itu berdasarkan
# scree plot, nilai eigen yang lebih besar dari satu hanya dicirikan 
# oleh 1 komponen.



# B -----------------------------------------------------------------------
print_eq(eig$vector, 1)

# Penjelasan :
# berdasarkan hasil poin a didapat proporsinya sebesar 75.47 %, 
# yang artinya minimal dapat menangkap 75.47 % keragaman dari data
# awal



# C -----------------------------------------------------------------------
korelasi <- 
  sweep(
    eig$vectors, 2, eig$values,
    FUN = function(a, b) a*sqrt(b)
  ) %>% 
  `dimnames<-`(
    list(paste0('y', 1:nrow(.)), paste0('x', 1:ncol(.)))
  )

# Untuk Y1
korelasi



lambda <- eig$values
e <- eig$vectors

S <- matrix(0, nrow = 4, ncol = 4)
for (i in 1:4) {
  S <- S + lambda[i] * e[, i] %*% t(e[, i]) 
}
S
