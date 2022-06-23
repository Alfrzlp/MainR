dat_sem <- readxl::read_xlsx('D:/tugas/dataPA_jatim.xlsx')
dat_sem %>% 
  glimpse()


# SEM ---------------------------------------------------------------------
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
# pak::pak('seminr')
glimpse(dat_sem)
library(seminr)

miskin_mm <- constructs(
  composite("kemiskinan", c('pm20', 'p1', 'p2')),
  composite("pendidikan", c('desasd', 'desasmp', 'desaslta', 'amh')),
  composite('demografi', c('tpt20', 'kp'))
  # composite("kesehatan", c('desapskms', 'desaapotek', 'desapoliklinik'))
)

miskin_mm <- as.reflective(miskin_mm)

#Creating structural model
#path disesuaikan dengan gambar model penelitian
miskin_sm <- relationships(
  paths(from = "pendidikan", to = c("kemiskinan")),
  paths(from = "demografi", to = c("kemiskinan"))
  # paths(from = "kesehatan", to = c("kemiskinan"))
  # paths(from = "sosial", to = c("kemiskinan"))
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

pt(3.54, 4 + 3, lower.tail = F)
pt(-0.373, 2 + 3, lower.tail = F)

pt(-3.036, 2 + 3, lower.tail = F)


# Rsquare
boot_miskin_model$rSquared 

plot(boot_miskin_model)





# -------------------------------------------------------------------------

dat_sem %>% 
  select(nmkab, pm20, p1, p2) %>% 
  pivot_longer(-nmkab) %>% 
  mutate(
    pos = rep(c(26, 5, 3), 38),
    name = factor(name, levels = c('pm20', 'p1', 'p2'))
  ) %>% 
  ggplot(aes(x = value, y = nmkab)) +
  geom_col(width = 0.7, fill = 'steelblue') +
  geom_text(
    aes(
      x = pos,
      label = paste(scales::comma(value, accuracy = 0.01), '  ')
    ),
    fontface = 2,
    nudge_x = 0,
    hjust = 0,
    size = 3
  ) +
  scale_y_discrete(
    labels = function(x) ifelse(
      str_detect(x, 'Kab'),
      str_remove_all(x, 'Kabupaten '),
      x
    )
  ) +
  labs(
    title = 'Indikator Kemiskinan Tahun 2020',
    subtitle = 'Provinsi Jawa Timur',
    x = NULL,
    y = 'Kabupaten/Kota'
  ) +
  theme(
    # panel.background = element_rect(fill = "white", colour = "black")
  ) +
  theme_bw() +
  facet_grid(
    ~name, scales = 'free_x', 
    labeller = as_labeller(
      c(
        'pm20' = 'Persentase Penduduk Miskin',
        'p1' = 'Indeks Kedalaman Kemiskinan',
        'p2' = 'Indeks Keparahan Kemiskinan'
      )
    )
  )


ggsave(
  filename = "E:/Visualisasi/kemiskinan_jatim.png",
  width = 10,
  height = 7,
  units = "in",
  dpi = 500,
  scale = 0.85,
  bg = "white"
)

