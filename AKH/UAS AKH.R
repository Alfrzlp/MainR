library(survival)
library(survminer)
library(tidyverse)

# Data --------------------------------------------------------------------
dat <- readxl::read_xlsx("D:/__SEMESTER 6/AKH/UAS AKH/Data_UAS_AKH_2022(1935).xlsx")
glimpse(dat)


dat <- dat %>%
  setNames(
    c(
      "id", "umur", "uk_tmr", "std_tmr", "n_simpul_tmr",
      "n_res_horA", "n_res_horB", "tgl_awal", "tgl_akhir", "jk",
      "terapi", "status"
    )
  ) %>%
  mutate(
    std_tmr = factor(std_tmr, levels = 1:3),
    jk = factor(jk, levels = c("Perempuan", "Laki-laki")),
    terapi = factor(terapi, levels = c("Tidak", "Ya")),
    status = ifelse(status == "Masih hidup", 0, 1),
    time = as.numeric(tgl_akhir - tgl_awal),
    kat_umur = case_when(
      # termasuk 0 dan 49
      between(umur, 0, 49) ~ "< 50 Tahun",
      umur >= 50 ~ "≥ 50 Tahun"
    ),
    kat_reshorA = case_when(
      between(n_res_horA, 0, 43) ~ "rendah",
      n_res_horA > 43 ~ "tinggi"
    ),
    kat_reshorB = case_when(
      between(n_res_horB, 0, 76) ~ "rendah",
      n_res_horB > 76 ~ "tinggi"
    )
  )
glimpse(dat)



dat %>%
  is.na() %>%
  colSums()


# Kaplan meier ------------------------------------------------------------
# kelompok umur
x <- 0.3
ggsurvplot(
  survfit(Surv(time, status) ~ kat_umur, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = "Time (days)",
  legend.title = "Kelompok Umur",
  legend.labs = c("< 50 Tahun", "≥ 50 Tahun")
)

# jumlah resepetor hormon A
x <- 0.1
ggsurvplot(
  survfit(Surv(time, status) ~ kat_reshorA, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = "Time (days)",
  legend.title = str_wrap("Kategori Jumlah Reseptor Hormon Type-A", 25),
  legend.labs = c("Tinggi", "Rendah")
)


# jumlah reseptor hormon B
ggsurvplot(
  survfit(Surv(time, status) ~ kat_reshorB, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = "Time (days)",
  legend.title = str_wrap("Kategori Jumlah Reseptor Hormon Type-B", 25),
  legend.labs = c("Tinggi", "Rendah")
)


# jenis kelamin
x <- 0.35
ggsurvplot(
  survfit(Surv(time, status) ~ jk, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = "Time (days)",
  legend.title = "Jenis Kelamin",
  legend.labs = c("Perempuan", "Laki-laki")
)

# terapi hormon
ggsurvplot(
  survfit(Surv(time, status) ~ terapi, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = "Time (days)",
  legend.title = "Status Menjalani Terapi Hormon",
  legend.labs = unique(dat$terapi)
)

# stadium tumor
ggsurvplot(
  survfit(Surv(time, status) ~ std_tmr, data = dat),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1),
  xlab = "Time (days)",
  legend.title = "Stadium Tumor",
  legend.labs = c(1:3)
)


# jumlah simpul tumor
ggplot(dat, aes(x = n_simpul_tmr, fill = factor(status))) +
  geom_histogram() +
  labs(x = "Jumlah Simpur Tumor", y = "Banyaknya Pasien") +
  theme(legend.position = "none") +
  facet_grid(
    ~status,
    labeller = as_labeller(
      c("0" = "Masih Hidup", "1" = "Meninggal")
    )
  )

# ukuran tumor
ggplot(dat, aes(x = uk_tmr, fill = factor(status))) +
  geom_histogram() +
  labs(x = "Ukuran Tumor", y = "Banyaknya Pasien") +
  theme(legend.position = "none") +
  facet_grid(
    ~status,
    labeller = as_labeller(
      c("0" = "Masih Hidup", "1" = "Meninggal")
    )
  )

# donut chart
hsize <- 2
dat %>% 
  group_by(status) %>% 
  summarise(n = n()) %>% 
  mutate(p = n/sum(n)) %>% 
  ggplot(aes(x = hsize, y = p, fill = factor(status))) +
  geom_col(color = "white", width = 0.8, size = 1.5) +
  geom_text(
    aes(label = scales::percent(p, accuracy = 0.01)),
    color = 'black',
    fontface = 2,
    position = position_stack(vjust = 0.5)
  ) +
  coord_polar(theta = "y") +
  xlim(c(1, hsize + 0.5)) +
  scale_fill_manual(
    'Status',
    values = c("#67CFC5", "#D9B05E"),
    labels = c('Masih Hidup', 'Meninggal')
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = NULL,
    fill = NULL
  ) +
  theme(
    plot.title = element_text(size = rel(1.3), face = "bold"),
    # legend.position = ,
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )




# Cox PH model ------------------------------------------------------------
mcox <- coxph(Surv(time, status) ~ kat_umur + jk + uk_tmr + std_tmr +
  n_simpul_tmr + kat_reshorA + kat_reshorB + terapi, data = dat)
summary(mcox)
AIC(mcox)
logLik(mcox)

# uji asumsi PH
# tolak Ho asumsi PH tidak terpenuhi
test.ph <- cox.zph(mcox)
test.ph
ggcoxzph(test.ph[6])


# stratified cox --------------------------------------------------------------
scox <- coxph(Surv(time, status) ~ kat_umur + jk + uk_tmr + std_tmr +
  n_simpul_tmr + kat_reshorB + terapi + strata(kat_reshorA),
data = dat
)
summary(scox)
AIC(scox)
cox.zph(scox)



scox_rendah <- coxph(Surv(time, status) ~ kat_umur + jk + uk_tmr + std_tmr +
  n_simpul_tmr + kat_reshorB + terapi,
data = dat %>% dplyr::filter(kat_reshorA == "rendah")
)
summary(scox_rendah)
AIC(scox_rendah)
logLik(scox_rendah)[1]

scox_tinggi <- coxph(Surv(time, status) ~ kat_umur + jk + uk_tmr + std_tmr +
  n_simpul_tmr + kat_reshorB + terapi,
data = dat %>% dplyr::filter(kat_reshorA == "tinggi")
)
summary(scox_tinggi)
AIC(scox_tinggi)
logLik(scox_tinggi)[1]






# Model Parametrik --------------------------------------------------------
# log normal
mlnorm <- survreg(Surv(time, status) ~ jk + kat_umur + uk_tmr + std_tmr +
  n_simpul_tmr + kat_reshorA + kat_reshorB + terapi,
data = dat,
dist = "lognormal"
)
summary(mlnorm)
AIC(mlnorm)
logLik(mlnorm)[1]


# log logistic
mllogis <- survreg(Surv(time, status) ~ jk + kat_umur + uk_tmr + std_tmr +
  n_simpul_tmr + kat_reshorA + kat_reshorB + terapi,
data = dat,
dist = "loglogis"
)
summary(mllogis)
AIC(mllogis)
logLik(mllogis)[1]




# -------------------------------------------------------------------------

all_dist <- 
  c("extreme", "logistic", "gaussian", "weibull", "exponential", 
    "rayleigh", "loggaussian", "lognormal", "loglogistic", "t")

hasil <- lapply(
  all_dist,
  function(x){
    # ganti time, status dan data saja
    x <- survreg(Surv(time, status) ~ 1, data = dat, dist = x)
    return(AIC(x))
  })

# aic semua model
data.frame(
  dist = unlist(all_dist),
  aic = unlist(hasil)
) %>% 
  arrange(aic) 



save_plot <- function(name){
  ggsave(
    filename = str_glue('E:/Visualisasi/tugas/UAS AKH/{name}.png'),
    dpi = 500,
    width = 7,
    height = 4.5,
    scale = 0.9,
    bg = 'white'
  )
}