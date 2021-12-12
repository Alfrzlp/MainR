library(tidyverse)

olah <- function(mat){
  print(mat)
  vec = as.vector(mat) %>% factorial()
  a = factorial(colSums(mat)[1])*factorial(colSums(mat)[2])*factorial(rowSums(mat)[1])*factorial(rowSums(mat)[2])
  p = a/(factorial(sum(mat))*vec[1]*vec[2]*vec[3]*vec[4])
  cat(p, "\n")
  return(p)
}

fisher.test2 <- function(m){
  mat = m
  pvalue = c(olah(mat))
  # ifelse(which.min(m) %in% c(2, 3), j <- -1, j <-  1)
  if(which.min(m) %in% c(2, 3)){
    j <- -1
    a = min(m[c(1,4)])
  }else{
    j <- 1
    a = min(m[c(2,3)])
  }
  # kurangi dulu
  for(i in 1:min(m)){
    mat = mat - j*matrix(c(1,-1,-1,1), 2, 2, byrow = T)
    pvalue = c(pvalue, olah(mat))
  }
  print('a')
  mat = m
  for(i in 1:a){
    mat = mat + j*matrix(c(1,-1,-1,1), 2, 2, byrow = T)
    pvalue = c(pvalue, olah(mat))
  }
  return(sum(pvalue[pvalue <= pvalue[1]]))
}


b = matrix(c(9, 5, 6, 2), 2, byrow = T)
b
fisher.test(b)
fisher.test2(b)


