library(dm)
d <- str2vec(str_replace_all(s2, '\n', ' '))
d

ordo <- length(d[d == 1])
for (i in 1:(ordo - 1)) {
  d <- append(d, rep(0, i), after = i*3)
}
mat <- matrix(d, byrow = T, nrow = ordo)
mat[lower.tri(mat)] <- mat[upper.tri(mat)]
mat

print_eq <- function(eig_vec, comp = 1:ncol(eig_vec)){
  for (i in comp) {
    cat(stringr::str_glue('y{i} = {paste0(round(eig_vec[,i], 3), "*X", 1:length(eig_vec[,i]), collapse = " + ")}'), '\n')
  }
}
read_mat <- function(s){
  d <- str_replace_all(s, '−', '-')
  d <- dm::str2vec(str_replace_all(s, '\n', ' '))
  ordo <- length(d[d == 1])
  for (i in 1:(ordo - 1)) {
    d <- append(d, rep(0, i), after = i*3)
  }
  mat <- matrix(d, byrow = T, nrow = ordo)
  mat[lower.tri(mat)] <- mat[upper.tri(mat)]
  mat
}
# 17/18 No 1 -------------------------------------------------------------------
s <- '1 0,81 -0,72
      1 -0,61
      1'

s2 <- '1 0,12 -0,23
       1 -0,05
       1'

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
s <- '0,00 5,27 55,03 56,25 41,67 42,50
5,27 0,00 55,77 55,57 44,52 45,32
55,03 55,77 0,00 49,60 24,89 23,93
56,25 55,57 49,60 0,00 49,06 54,58
41,67 44,52 24,89 49,06 0,00 6,73
42,50 45,32 23,93 54,58 6,73 0,00'

mat_jarak <- 
  read_string(s) %>% 
  separator_convert(everything()) %>% 
  `colnames<-`(1:ncol(.)) %>% 
  `rownames<-`(1:nrow(.)) %>% 
  as.matrix()

mat_jarak









aglo_clust <- function(mat_jarak, method = 'single'){
  method <- match.arg(stringr::str_to_lower(method), c('single linkage', 'complete linkage', 'average linkage'))
  if (method == 'single linkage') {
    fun_method <- min
  } else if (method == 'complete linkage') {
    fun_method <- max
  } else if (method == 'average linkage') {
    fun_method <- mean
  }
  cli::cli_div(
    theme = list(span.emph = list(color = "orange", fontweight = "bold"))
  )
  cli::cli_alert_success('Metode : {.emph {method}}')
  
  i <- 1
  while (nrow(mat_jarak) != 2) {
    nilai_min <- min(mat_jarak[upper.tri(mat_jarak)])
    pos_min <- which(mat_jarak == nilai_min, arr.ind = T)
    idx <- as.numeric(pos_min[,'col'])
    
    
    new_name <- paste(rownames(pos_min), collapse = ', ')
    # new_value <- apply(mat_jarak[-idx, idx], 1, FUN = min)
    new_value <- tryCatch(
      {
        new_value <- apply(mat_jarak[-idx, idx], 1, FUN = fun_method)
      },
      error = function(e){
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

    cli::cli_h1('Iterasi ke-{i}')
    if (i == 1) {
      cli::cli_text(cli::col_br_cyan('Matriks jarak Awal :'))
      print(knitr::kable(mat_jarak))
      cat('\n')
    }
    
    cli::cli_text('Nilai Minimal : {.emph {nilai_min}} Pada {.emph {rownames(pos_min)[1]}} dengan {.emph {rownames(pos_min)[2]}}')
    
    cli::cli_text(cli::col_br_cyan('Matriks jarak Baru :\n'))
    print(knitr::kable(new_mat))
    
    i <- i + 1
    mat_jarak <- new_mat
  }
}


aglo_clust(mat_jarak = mat_jarak)
aglo_clust(mat_jarak = mat_jarak, method = 'compl')




match.arg('singl', c('single linkage', 'complete linkage', 'average linkage'))
cli::cli_h3(cli::col_br_('Matriks jarak Awal :'))
cli::cli_div(theme = list(span.emph = list(color = "orange", strong = T)))
cli::cli_alert_success('\n\nNilai Minimal : {.emph {nilai_min}} ')









cli::cli_alert_success('Metode : Single')
matrix(mat_jarak[-idx, idx], nrow = 1) %>% 
  `colnames<-`(names(mat_jarak[-idx, idx])) %>% 
  `rownames<-`(rownames(mat_jarak)[-idx]) %>% 
  apply(1, FUN = min)

# 1 2 3 4 5 6
# 1 2 4 3 5 6
# 
# 1 2 3 4 5 6 7 8 9 10
# 1 2 5 3 6 8 4 7 9 10


