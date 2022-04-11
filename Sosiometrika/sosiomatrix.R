library(tidyverse)
library(readxl)

absensi <- read_xlsx('D:/__SEMESTER 6/absensi3SK3.xlsx')
head(absensi)

dat <- read_xlsx("D:/__SEMESTER 6/___buat tugas/sosio.xlsx")
dat <- dat %>% 
  dplyr::select(-c(1, 11:12)) %>% 
  `colnames<-`(c('no', 'nim', 'nama', 'sig1', 'sig2', 'apg1', 'apg2', 'akh1', 'akh2')) %>% 
  mutate(
    no = as.numeric(no)
  ) %>% 
  mutate_at(
    vars(sig1:akh2),
    ~ factor(.x, levels = absensi$nama, labels = absensi$no)
  ) %>% 
  arrange(no) 

head(dat)


tb <- table(dat$no, dat$sig1)
tb[tb == 1] <- 2
tb

tb2 <- table(dat$no, dat$sig2)
tb2

tb + tb2


socio_mat <- function(src, target, value = rev(1:length(target))){
  result <- table(1:length(src), 1:length(src))
  for (i in 1:length(target)) {
    tb <- table(src, target[[i]])
    tb[tb == 1] <- value[i]
    result <- result + tb
  }
  diag(result) <- 0
  return(result)
}


socio_mat(dat$no, list(dat$akh1, dat$akh2)) %>% 
  dm::copy2c()
