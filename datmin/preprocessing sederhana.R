library(tidyverse)
library(inspectdf)
library(tidymodels)
library(lubridate)
tidymodels_prefer()


# Data --------------------------------------------------------------------
dat <- read.csv("D:/__Datasets/ml/data survival delirium latihan_1.csv")
glimpse(dat)



all_date <- unique(c(dat$tglmask, dat$tgldelir, dat$tglplg))
dmy(all_date, locale="id-ID")

all_problem <- all_date[is.na(dmy(all_date, locale="id-ID"))]
all_problem

dmy(get_cleanDate(all_date), locale="id-ID")[628]



# Perbaikan Tanggal -------------------------------------------------------
# all locale https://stackoverflow.com/questions/3191664/list-of-all-locales-and-their-short-codes

get_cleanDate <- function(x){
  x <- str_replace_all(x, "jsn", "jan")
  x <- str_replace_all(x, "agt", "agu")
  x <- str_replace_all(x, "fen", "feb")
  x <- str_replace_all(x, "\\-?0kt", "okt")
  x <- str_replace_all(x, "spt", "sep")
  x <- str_replace_all(x, "junii", "juni")
  x <- str_replace_all(x, "ju i|uli|ju;li", " juli")
  x <- str_replace_all(x, "aprill", "april")
  x <- str_replace_all(x, "maaret|mart", "maret")
  x <- str_replace_all(x, "meui", "mei")
  return(x)
}


dat$tgldelir <- get_cleanDate(dat$tgldelir)
dat$tglmask <- get_cleanDate(dat$tglmask)
dat$tglplg <- get_cleanDate(dat$tglplg)

dat <- dat %>% 
  mutate_at(vars(starts_with("tgl")), ~ lubridate::dmy(.x, locale = "id-ID"))
glimpse(dat)




# ubah tipe data kolom ----------------------------------------------------
# melihat semua kategori dari setiap variabel kecuali variabel
# yang berawalan tgl
apply(select(dat, !starts_with('tgl')), 2, function(x) table(x))


# Daftar Permasalahan

# 1. status nikah
table(dat$statusnikah)
# status nikah = 2.7 apakah noise ? 
# karena hanya ada satu orang maka bisa dibuang

# 2. tipe data kategorik spt jenis kelamin, 
# di R sebaiknya menggunakan factor bukan numeric / integer
glimpse(dat)

# semua kolom akan diubah menjadi factor kecuali variabel
# TIME, timedelirium, lamaperawatan, usia, tglmask, tgldelir, tglplg
# karena variabel tsb skala rasio dan tgl harusnya bertipe date



dat <- dat %>% 
  # buang noise
  filter(statusnikah != 2.7) %>% 
  # ubah semua kolom menjadi factor kecuali TIME, timedelirium dll
  mutate_at(
    vars(-c(TIME, timedelirium, lamaperawatan, usia, tglmask, tgldelir, tglplg)),
    ~ factor(.x)
  ) %>% 
  # nama kolom menjadi lower case semua, lebih mudah untuk pengolahan
  janitor::clean_names()

# abaikan warning. 

glimpse(dat)



# -------------------------------------------------------------------------
# hapus salah satu kolom karena duplikat, cuma beda kategorisasi
dat$jeniskelamin
dat$jenkelbaru

dat <- dat %>% 
  select(-jeniskelamin)



# jumlah NA dan persentasenya ---------------------------------------------
inspect_na(dat) %>% 
  dplyr::filter(cnt > 0)

# persentase NA yang terlalu banyak jika diimputasi
# akan membuat data jelek. misalnya timedelirium. 



# imputasi NA -------------------------------------------------------------
rec <- recipe(time ~ ., dat) %>% 
  # kolom tipe nominal (yaitu factor atau charcter) akan di imputasi dengan modus
  step_impute_mode(all_nominal_predictors())
rec <- rec %>% prep(dat)



# Data setelah diimputasi
df_clean <- bake(rec, dat)
glimpse(df_clean)


# kondisi NA saat ini
inspect_na(df_clean) 




# anda bisa mencoba imputasi lain seperti dengan KNN, 
# cukup ganti dengan
step_impute_knn(all_nominal_predictors())


