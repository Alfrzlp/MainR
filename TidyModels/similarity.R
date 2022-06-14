res <- c()
for(i in 1:nrow(df_test)){
  similarity <- c()
  for(j in 1:nrow(df_train)){
    similarity <- c(
      similarity,
      sum(df_test[i, 2:11] == df_train[j, 2:11])
    )
  }
  idx <- which.max(similarity)
  res <- c(res, df_train$class_asd[idx])
  cat('Baris ke -', i, ' Selesai\n')
}

res

hasil <- 
  df_sub %>% 
  mutate(`Class/ASD` = res)