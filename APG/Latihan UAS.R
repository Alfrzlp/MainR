library(dm)

aglo_clust <- function(mat_jarak, method = "single") {
  mat_jarak <- `colnames<-`(mat_jarak,  1:ncol(mat_jarak))
  mat_jarak <- `rownames<-`(mat_jarak,  1:nrow(mat_jarak))
  method <- match.arg(stringr::str_to_lower(method), c("single linkage", "complete linkage", "average linkage"))
  if (method == "single linkage") {
    fun_method <- min
  } else if (method == "complete linkage") {
    fun_method <- max
  } else if (method == "average linkage") {
    # fun_method <- mean
    stop('Metode dalam pengembangan')
  }
  cli::cli_div(
    theme = list(span.emph = list(color = "orange", fontweight = "bold"))
  )
  cli::cli_alert_success("Metode : {.emph {method}}")
  
  i <- 1
  while (nrow(mat_jarak) != 2) {
    nilai_min <- min(mat_jarak[upper.tri(mat_jarak)])
    pos_min <- which(mat_jarak == nilai_min, arr.ind = T)
    idx <- as.numeric(pos_min[, "col"])
    
    
    new_name <- paste(rownames(pos_min), collapse = ", ")
    # new_value <- apply(mat_jarak[-idx, idx], 1, FUN = min)
    new_value <- tryCatch(
      {
        new_value <- apply(mat_jarak[-idx, idx], 1, FUN = fun_method)
      },
      error = function(e) {
        new_value <- matrix(mat_jarak[-idx, idx], nrow = 1) %>%
          `colnames<-`(names(mat_jarak[-idx, idx])) %>%
          `rownames<-`(rownames(mat_jarak)[-idx]) %>%
          apply(1, FUN = fun_method)
        return(new_value)
      }
    )
    
    new_value <- c(0, new_value)
    names(new_value)[1] <- new_name
    
    new_mat <- rbind(new_value[-1], mat_jarak[-idx, -idx])
    new_mat <- cbind(new_value, new_mat)
    colnames(new_mat) <- names(new_value)
    
    cli::cli_h1("Iterasi ke-{i}")
    if (i == 1) {
      cli::cli_text(cli::col_br_cyan("Matriks jarak Awal :"))
      print(knitr::kable(mat_jarak))
      cat("\n")
    }
    
    cli::cli_text("Nilai Minimal : {.emph {nilai_min}} Pada {.emph {rownames(pos_min)[1]}} dengan {.emph {rownames(pos_min)[2]}}")
    
    cli::cli_text(cli::col_br_cyan("Matriks jarak Baru :\n"))
    print(knitr::kable(new_mat))
    
    i <- i + 1
    mat_jarak <- new_mat
  }
}
print_eq <- function(eig_vec, comp = 1:ncol(eig_vec)) {
  for (i in comp) {
    cat(stringr::str_glue('y{i} = {paste0(round(eig_vec[,i], 3), "*X", 1:length(eig_vec[,i]), collapse = " + ")}'), "\n")
  }
}
read_mat <- function(s) {
  d <- str_replace_all(s, "−", "-")
  d <- dm::str2vec(str_replace_all(s, "\n", " "))
  ordo <- length(d[d == 1])
  for (i in 1:(ordo - 1)) {
    d <- append(d, rep(0, i), after = i * 3)
  }
  mat <- matrix(d, byrow = T, nrow = ordo)
  mat[lower.tri(mat)] <- mat[upper.tri(mat)]
  mat
}
to_matrix <- function(x, ordo) {
  for (i in 1:(ordo - 1)) {
    x <- append(x, values = rep(0, i), after = ordo * i)
  }
  R <- matrix(x, nrow = ordo, byrow = T)
  R[lower.tri(R)] <- matrix(x, nrow = ordo, byrow = F)[lower.tri(R)]
  R
}

# 16/17 No 1 --------------------------------------------------------------
# pca
# eigen value dari data
# eigen value = proporsi keragaman yang dijelaskan komponen

# korelasi kanonik
# eigen value dari R11_I_sqrt %*% R12 %*% solve(R22) %*% R21 %*% R11_I_sqrt
# merupakan rho^2
# rho = korelasi kanonik


# analisis cluster
# jarak digunakan untuk membentuk matriks jarak, dari matriks jarak
# dilakukan pengabungan sehingga membentuk cluster@


# 16/17 No 2 --------------------------------------------------------------
n <- 40
xbar <- c(46.1, 57.3, 50.4)
S <- matrix(c(101.3, 63, 71, 63, 80.2, 55.6, 71, 55.6, 97.4), 3, byrow = T)
S

eig <- eigen(S)

cumsum(eig$values / sum(eig$values))
# tidak bisa karena butuh indikator ke3 juga
print_eq(eig$vectors, 1)



# 17/18 No 1 -------------------------------------------------------------------
s <- "1 0,81 -0,72
      1 -0,61
      1"

s2 <- "1 0,12 -0,23
       1 -0,05
       1"

R1 <- read_mat(s)
R2 <- read_mat(s2)

# nilai eigen
eig <- eigen(R1)
# nilai proporsi
eig$values / sum(eig$values)
# nilai kumulatif proporsi
cumsum(eig$values / sum(eig$values))

# didapat 1 komponen saja
# persamaannya
print_eq(eig$vectors, 1)


# 17/18 No 2 --------------------------------------------------------------
s <- "0,00 5,27 55,03 56,25 41,67 42,50
5,27 0,00 55,77 55,57 44,52 45,32
55,03 55,77 0,00 49,60 24,89 23,93
56,25 55,57 49,60 0,00 49,06 54,58
41,67 44,52 24,89 49,06 0,00 6,73
42,50 45,32 23,93 54,58 6,73 0,00"

mat_jarak <-
  read_string(s) %>%
  separator_convert(everything()) %>%
  `colnames<-`(1:ncol(.)) %>%
  `rownames<-`(1:nrow(.)) %>%
  as.matrix()

mat_jarak


aglo_clust(mat_jarak = mat_jarak)
aglo_clust(mat_jarak = mat_jarak, method = "compl")




# 1 2 3 4 5 6
# 1 2 4 3 5 6
#
# 1 2 3 4 5 6 7 8 9 10
# 1 2 5 3 6 8 4 7 9 10





# 18/19 No 1 --------------------------------------------------------------

R <- c(
  1, 0.88, 0.96, 0.94, 0.9, 0.9,
  1, 0.9, 0.91, 0.94, 0.95,
  1, 0.96, 0.93, 0.92,
  1, 0.93, 0.94,
  1, 0.95,
  1
)

R
# 2
# 3, 8

R <- to_matrix(R, 6)
R == t(R)

eig <- eigen(R)
eig

eig$values / sum(eig$values)
cumsum(eig$values / sum(eig$values))

print_eq(eig$vectors, 1)

# 18/19 No 2 --------------------------------------------------------------
s <- "0,163
0,286
0,290
0,896
0,352
−0,218
0,384
0,863
0,889
31,73
−0,425
0,421
−0,736
0,127
0,427
0,632
−0,676
0,104
0,231
54,13
−0,443
0,566
0,507
−0,218
0,384
0,354
0,528
−0,175
−0,170
70,13
0,625
0,366
−0,110
−0,153
0,529
−0,307
−0,098
−0,144
−0,145
81,02
0,388
0,307
0,028
0,025
−0,411
0,394
0,029
0,085
−0,040
87,44
0,233
−0,438
0,067
0,008
0,308
0,421
0,099
0,002
0,005
93,36
0,029
0,019
0,149
0,222
−0,024
0,022
−0,122
−0,410
0,181
96,6
−0,013
−0,002
0,270
0,020
0,031
0,005
−0,281
0,135
−0,126
98,65
0,018
−0,003
0,066
−0,243
−0,016
0,003
−0,055
0,015
0,236
100"

dat <- read_string(s) %>%
  separator_convert(V1, "−", "-", to_numeric = F) %>%
  separator_convert(V1) %>%
  pull() %>%
  matrix(nrow = 10, ncol = 9, byrow = F) %>%
  as.data.frame() %>%
  slice(-10)

eig <- dat %>%
  .^2 %>%
  colSums()
eig

cumsum(eig / sum(eig))

# comunality
dat[, 1:4] %>%
  rowwise() %>%
  mutate(com = sum(across(V1:V4)^2))



# 18/19 No 4 --------------------------------------------------------------

Ry <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, byrow = T)
Rx <- matrix(c(1, 0.37, 0.21, 0.37, 1, 0.35, 0.21, 0.35, 1), nrow = 3, byrow = T)
Rxy <- matrix(c(0.56, 0.43, 0.67, 0.59, 0.84, 0.74), nrow = 3, byrow = T)
Ryx <- t(Rxy)
Ry
Rx
Rxy
Ryx

eigR11 <- eigen(Ry)
eigR22 <- eigen(Rx)

# R ^ -1/2
R11_I_sqrt <- degree_sigma(eigR11, -1 / 2)
R11_I_sqrt
R22_I_sqrt <- degree_sigma(eigR22, -1 / 2)
R22_I_sqrt


M <- R11_I_sqrt %*% R12 %*% solve(R22) %*% R21 %*% R11_I_sqrt
M

rho2 <- eigen(M)$values
rho2
rho <- sqrt(rho2)
rho


eig <- eigen(M)$vector

# U1
u1 <- R11_I_sqrt %*% eig[, 1]
u1

# 20/21 No 1 --------------------------------------------------------------
R <- matrix(c(1.00, 0.90, 0.74, 0.64, 0.90,
              1.00, 0.33, 0.74, 0.74, 0.33,
              1.00, 0.66, 0.64, 0.74, 0.66, 1.00), 4)
R

eig <- eigen(R)
eig$values
eig$values / sum(eig$values)
cumsum(eig$values / sum(eig$values))

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
    theme_bw() +
    theme(
      plot.title = element_text(face = 'bold')
    )
}

scree_plot(eig$values)
eig$vectors

# korelasi
lambda <- eig$values
i <- 1 # y
k <- 1 # x

eig$vectors[i, k] * sqrt(lambda[i]) / sqrt(R[k, k])
eig$vectors[i, k] * sqrt(lambda[i]) / sqrt(R[k, k])

# n_komponen
n_comp <- 2
sweep(
  eig$vectors[, 1:n_comp], 2, eig$values[1:n_comp],
  FUN = function(a, b) a*sqrt(b)
) %>% 
  `dimnames<-`(
    list(paste0('x', 1:nrow(.)), paste0('y', 1:ncol(.)))
  )

# 20/21 No 2 --------------------------------------------------------------

xbar <- c(92.3874, 69.1226, .3318, 36288.779)
a <- c(-24.286, 0.219, 0.021, 7.037, 0)
asal <- c(1, 95.4, 68.2, 0.29, 541934.5)
pemekaran <- c(1, 97.8, 73.1, 0.33, 680148.0)

sum(a*asal)
sum(a*pemekaran)

sum(centroid)/2



x <- matrix(c(0, 1, 11, 5,
         1, 0, 2, 3,
         11, 2, 0, 4,
         5, 3, 4, 0), byrow = T, nrow = 4)

aglo_clust(x)
aglo_clust(x, method = 'complete')
aglo_clust(x, method = 'ave')
