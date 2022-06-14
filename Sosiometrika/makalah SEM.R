dat_sem <- readxl::read_xlsx('D:/tugas/dataPA_jatim.xlsx') %>% 
  select(-c(tpt21, pm21))

dat_sem %>% 
  glimpse()


library(lavaan)

full_mod = '
# latent variable definitions
  pendidikan =~ amh + rls
  demografi =~  rk + kp
  miskin =~ pm20 + tpt20

# regressions
  pendidikan ~ demografi
  miskin ~ demografi + pendidikan
'

mod.est <- sem(
  model = full_mod, 
  data = dat_sem[, -1]
)

summary(mod.est, rsquare = T)
summary(mod.est, fit.measures = TRUE)

varTable(mod.est)
lavInspect(mod.est, "cov.lv")

fitmeasures(mod.est, c('GFI', 'cfi', 'NFI', 'NNFI','SRMR'))


# SEM PLS -----------------------------------------------------------------
pak::pak('seminr')
glimpse(dat_sem)
library(seminr)

miskin_mm <- constructs(
  composite("kemiskinan", c('pm20', 'p1', 'p2')),
  composite("pendidikan", c('hls', 'amh')),
  composite('demografi', c('rk', 'kp')),
  composite("tenaga", c('tpt20'))
)

miskin_mm <- as.reflective(miskin_mm)

#Creating structural model
#path disesuaikan dengan gambar model penelitian
miskin_sm <- relationships(
  paths(from = "pendidikan", to = c("kemiskinan", 'tenaga')),
  paths(from = "tenaga", to = c("kemiskinan")),
  paths(from = "demografi", to = c("kemiskinan", 'tenaga'))
)


miskin_pls <- estimate_pls(
  data = dat_sem[, -1],
  measurement_model = miskin_mm,
  structural_model = miskin_sm
)

model_summary <- summary(miskin_pls)
model_summary


plot_scores(miskin_pls)
miskin_pls$rSquared

# Evaluasi Model Pengukuran (Outter Model) --------------------------------
# Convergent validity -----------------------------------------------------
# 1.  Dengan nilai loading
model_summary$loadings
# nilai loading factor yang < 0.5
# maka variabel dapat dihilangkan

# jika sudah > 0.5 semua, maka dapat dikatakan
# masing-masing indikator sudah dapat menjelaskan
# variabel latennya sebesar lebih dari 50% atau dengan
# kata lain sudah memiliki validitas yang baik dalam
# mengukur variabel latennya

# 2. Dengan AVE
model_summary$reliability
# Didapatkan nilai AVE setiap variabel laten lebih dari 0,5, 
# yang menandakan semua variabel valid. Diartikan juga bahwa,
# masing-masing variabel laten telah menunjukkan ukuran convergent 
# validity yang baik karena secara umum dapat menjelaskan rata-rata
# lebih dari setengah varians indikator.

# composite reliability ---------------------------------------------------
# rhoC = composite reliability
model_summary$reliability



# Diskriminant validity ---------------------------------------------------
# nilai Fornell-Larcker
model_summary$validity$fl_criteria

# Didapatkan nilai korelasi suatu variabel laten dengan
# variabel laten itu sendiri lebih besar daripada nilai korelasi
# variabel laten dengan variabel laten lainnya. Hal ini berarti 
# model yang digunakan memenuhi Fornell-Larcker criterion, 
# sehingga dapat dikatakan model tersebut telah memiliki 
# discriminant validity yang baik.









# Evaluasi Model  (Inner Model) ---------------------------------
boot_miskin_model <- bootstrap_model(
  seminr_model = miskin_pls,
  nboot = 1000, cores = 3, 
  seed = 1
)

boot_summary <- summary(boot_miskin_model)
boot_summary

# df = 4
# df = n variabel + n variabel
boot_summary$bootstrapped_paths
boot_summary$bootstrapped_total_paths



# Rsquare
boot_miskin_model$rSquared 

plot(boot_miskin_model)









mat <- matrix(sample(1:100, 9), 3, 3)
X <- mat <- nitip

X <- sweep(X, MARGIN = 2, STATS = colMeans(X), FUN = '-')
X <- sweep(X, MARGIN = 1, STATS = rowMeans(X), FUN = '-')


# dikurangi average row
X <- sweep(X, MARGIN = 1, STATS = X[, ncol(X)], FUN = '-')
# dikurangi average col
X <- sweep(X, MARGIN = 2, STATS = X[nrow(X), ], FUN = '-')
X


nitip <- mat
for (r in 1:(nrow(mat)-1) ){
  for (c in 1:(ncol(mat)-1) ) {
    mat[r,c] <- mat[r,c]-mat[r,3]-mat[3,c]
  }
}
mat
nitip
