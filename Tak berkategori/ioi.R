library(readxl)
library(tidyverse)

lf <- read_xlsx('D:/Telegram/Data simulasi_content.xlsx', sheet = 1)
pkl <- read_xlsx('D:/Telegram/Data simulasi_content.xlsx', sheet = 2)

lf %>% glimpse()
pkl %>% glimpse()


# Function ----------------------------------------------------------------
# NSR
nsr <- function(tabel){
  return((tabel[2]-tabel[4])*100/(tabel[1]+tabel[3]))
}
# Index of inconsistency
ioi <- function(tabel){
  n <-  sum(colSums(tabel))
  df <- 
    data.frame(
      kategori = colnames(tabel),
      y.i = colSums(tabel),
      yi. = rowSums(tabel),
      yii = diag(tabel)
    ) %>% 
    mutate(
      ioi = (y.i + yi. - 2*yii)*100*n/(y.i*(n-yi.) + yi.*(n-y.i))
    )
  
  # Aggregate index of inconsistency
  iag <- (n - sum(df$yii))*100/(n - sum(df$y.i*df$yi.)/n)
  return(list(df, iag = iag))
}


# Jenis kelamin -----------------------------------------------------------
tab_jk <- 
  lf %>% 
  left_join(pkl, by = 'bsruta', suffix = c('_lf', '_pkl')) %>% 
  select(starts_with('jenis_kel')) %>% 
  table() 

tab_jk

nsr(tab_jk)
ioi(tab_jk)

# pendidikan --------------------------------------------------------------
tab_pend <- 
  lf %>% 
  left_join(pkl, by = 'bsruta', suffix = c('_lf', '_pkl')) %>% 
  select(starts_with('Pendi')) %>% 
  drop_na() %>% 
  table() 

tab_pend
ioi(tab_pend)



lf %>% 
  left_join(pkl, by = 'bsruta', suffix = c('_lf', '_pkl')) %>%
  select(bsruta, nama_krt_lf, starts_with('nama'))

