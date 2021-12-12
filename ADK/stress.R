s <- '1	0	0	0	0	0	0	0	0	0	1	0	1	0
1	1	3	3	2	2	2	2	2	1	1	1	1	2
0	0	1	1	0	0	1	0	0	3	1	0	1	0
1	0	2	1	2	0	1	3	0	2	1	2	1	1
1	1	0	1	0	1	1	0	1	1	0	1	0	0
1	1	1	1	1	1	1	1	1	2	0	0	1	1
1	1	0	1	2	1	1	1	1	3	1	1	0	1'

read.table(textConnection(s), header = F) %>% 
  t() %>% as.data.frame() %>% 
  remove_rownames() %>% 
  colSums()



# Data --------------------------------------------------------------------

df <- readxl::read_xlsx('C:/Users/Ridson Alfarizal/Downloads/Quesioner Surcon Kel.1 (Jawaban) (1).xlsx')
df %>% glimpse()

ap <- paste0('ap', 1:14)
col_name <- 
  c('waktu', 'jabatan', 'lama_rapat', 'n_rapat', 'n_tugas',
    'ikut_org','n_org', 'lama_rapat_org', 'n_rapat_org',
    ap, 'absen', 'kelas')



hasil <- 
  df %>% 
  `colnames<-`(col_name) %>% 
  dplyr::select(-waktu) %>% 
  mutate(
    ikut_org = if_else(ikut_org == 'Tidak', 0, 1),
    kelas = str_trim(kelas, 'both'),
    ta = if_else(str_detect(kelas, 'D3'), 1, 0)
  ) %>% 
  distinct(.keep_all = T) %>% 
  dplyr::filter(
    !(absen %in% c(4, 5, 11, 33) & kelas == '3SK3')
  ) %>% 
  mutate_at(
    vars(n_org, lama_rapat_org, n_rapat_org),
    ~ if_else(ikut_org == 0, 0, .x)
  ) %>% 
  rowwise() %>% 
  mutate(
    stress = sum(c_across(starts_with('ap')))
  ) %>% 
  ungroup() %>% 
  mutate(
    kat_stress = case_when(
      between(stress, 0, 14)~'Normal',
      between(stress, 15, 18)~'Ringan',
      between(stress, 19, 25)~'Sedang',
      between(stress, 26, 33)~'Parah',
      stress >= 34 ~'Sangat Parah'
    ),
    kat_stress = factor(kat_stress,
                        levels = c('Normal', 'Ringan', 'Sedang', 'Parah', 'Sangat Parah'),
                        ordered = T),
    jabatan = factor(jabatan,
                     levels = c('Anggota', 'Sekben subseksi/subdivisi', 'Sekben seksi/divisi - Koor subsesksi/subdivisi','BPH - Koor Riset/Bidang', 'BPH 5'),
                     ordered = T)
  ) %>% 
  dplyr::select(-c(starts_with('ap'), absen, kelas))


head(hasil)
dim(hasil)

# Visualisasi -------------------------------------------------------------
hasil %>% 
  glimpse()

table(hasil$jabatan, hasil$kat_stress) %>% 
  as.matrix() %>% 
  vcdExtra::GKgamma()

cor(as.numeric(hasil$jabatan),
    as.numeric(hasil$kat_stress), method = 'kendal')


library(plotly)
pie <- hasil %>% 
  group_by(kat_stress) %>% 
  count()

plot_ly(
  pie,
  type='treemap',
  values = ~n,
  labels = ~ paste(kat_stress, '\n', scales::percent(n/sum(n))),
  insidetextfont = list(size = 19),
  marker = list(colors = viridis::viridis(5)),
  parents = NA
)

hasil %>% 
  group_by(kat_stress) %>% 
  count() %>% 
  mutate(
    ypos = cumsum(n)- 0.5*n
  ) %>% 
  ggplot(aes(x="", y=n, fill=kat_stress)) +
  geom_bar(stat="identity", width=1, color="white") +
  # geom_text(aes(y = c(90, 60, 30, 14, 5),
  #               label = scales::percent(n/sum(n))),
  #           color = "white", size=5) +
  coord_polar("y", start=0) +
  # labs(
  #   title = 'Proporsi Tingkat Stress Mahasiswa STIS Tingkat III'
  # ) +
  theme_void() +
  scale_fill_viridis_d('Kategori Stress') +
  theme(
    title = element_text(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank()
  ) 
  geom_text(aes(y = n/5 + c(0, cumsum(n)[-length(n)]), label = kat_stress), color = "white", size=5) 


# Kategori stress dengan jabatan
hasil %>% 
  group_by(kat_stress, jabatan) %>% 
  count() %>% 
  ggplot(aes(y = reorder(kat_stress, n), x = n, fill = jabatan)) +
  geom_bar(position="fill", stat="identity")+
  labs(x = 'Proporsi Mahasiswa/i', y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = 'right',
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) +
  scale_x_continuous(labels = scales::percent)



# Rata2 lama rapat dalam seminggu
ggplot(hasil, aes(y = lama_rapat, x = kat_stress)) +
  geom_boxplot(aes(fill = kat_stress)) +
  scale_fill_brewer('Kategori Stress', palette="RdYlGn", direction = -1)+
  geom_jitter(shape=16, position=position_jitter(0.15)) + 
  labs(y = 'Rata-Rata Lama Rapat Dalam Seminggu (Jam)',
       x = NULL) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.y = element_line(),
    axis.line.y = element_line(size = 0.5),
    axis.line.x = element_line(size = 0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) 



# banyak rapat
ggplot(hasil, aes(y = n_rapat, x = kat_stress)) +
  geom_boxplot(aes(fill = kat_stress)) +
  geom_jitter(shape=16, position=position_jitter(0.1)) + 
  scale_fill_brewer('Kategori Stress', palette="RdYlGn", direction = -1)+
  labs(y = 'Banyaknya Rapat PKL Dalam Seminggu',
       x = NULL) +
  scale_y_continuous(labels = 0:7,
                     breaks = 0:7) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.y = element_line(),
    axis.line.y = element_line(size = 0.5),
    axis.line.x = element_line(size = 0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) 


# Kegiatan diluar PKL
ggplot(hasil, aes(y = n_tugas, x = kat_stress)) +
  geom_boxplot(aes(fill = kat_stress)) +
  scale_fill_brewer('Kategori Stress', palette="RdYlGn", direction = -1)+
  geom_jitter(shape=16, position=position_jitter(0.15)) +
  labs(y = 'Banyaknya Tugas Kuliah Dalam Seminggu',
       x = NULL) +
  scale_y_continuous(labels = 0:7,
                     breaks = 0:7) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.y = element_line(),
    axis.line.y = element_line(size = 0.5),
    axis.line.x = element_line(size = 0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


# Organisasi (perlu revisi)
hasil %>% 
  group_by(n_org, kat_stress) %>% 
  count() %>% 
  mutate(
    n_org = if_else(n_org >= 2, 'lebih dari 2', as.character(n_org))
  ) %>% 
  ggplot() +
  geom_bar(aes(x = n_org, y = n, fill = kat_stress),
               position="fill", stat="identity") +
  scale_fill_brewer('Kategori Stress', palette="RdYlGn", direction = -1)+
  labs(x = 'Banyaknya Organisasi yang Masih Diikuti',
       y = NULL) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian()  +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_text(size = 11),
    axis.ticks = element_line(),
    axis.line.y = element_line(size = 0.5),
    axis.line.x = element_line(size = 0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


# hasil %>% 
#   group_by(lama_rapat_org, kat_stress) %>% 
#   count() %>% 
#   ggplot() +
#   geom_bar(aes(x = lama_rapat_org, y = n, fill = kat_stress),
#            position="fill", stat="identity") +
#   scale_fill_brewer('Kategori Stress', palette="RdYlGn", direction = -1)+
#   labs(x = 'Lama Rapat Organisasi Dalam Seminggu (Jam)',
#        y = NULL) +
#   scale_x_continuous(labels = c(0, 1),
#                      breaks = c(0, 1))


hasil %>% 
  group_by(n_rapat_org, kat_stress) %>% 
  count() %>% 
  ggplot() +
  geom_bar(aes(x = n_rapat_org, y = n, fill = kat_stress),
           position="fill", stat="identity") +
  scale_fill_brewer('Kategori Stress', palette="RdYlGn", direction = -1)+
  labs(x = 'Banyaknya Rapat Organisasi Dalam Seminggu',
       y = NULL) +
  scale_x_continuous(labels = 0:4,
                     breaks = 0:4) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian() +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_text(size = 11),
    axis.ticks = element_line(),
    axis.line.y = element_line(size = 0.5),
    axis.line.x = element_line(size = 0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )



# Save --------------------------------------------------------------------
#  8 3
ggsave(
  'D:/__SEMESTER 5/Survei Contoh/plot/prop.png',
  width = 6,
  height = 4,
  limitsize = F,
  dpi = 300,
  type = "cairo-png",
  bg = 'transparent'
)

x = as.numeric(hasil$kat_stress)
x = hasil$stress
v = (N-112)*var(x)/(N*112)
sqrt(v)/mean(x)


# Olah data Untuk Regresi -------------------------------------------------
# df_hasil <- 
#   hasil %>% 
#   mutate(
#     jabatan = case_when(
#       jabatan == 'Anggota'~0,
#       jabatan == 'Sekben subseksi/subdivisi'~1,
#       jabatan == 'Sekben seksi/divisi - Koor subsesksi/subdivisi'~2,
#       jabatan == 'BPH - Koor Riset/Bidang'~3,
#       jabatan == 'BPH 5'~4
#     ),
#     kat_stress = case_when(
#       kat_stress == 'Normal'~0,
#       kat_stress == 'Ringan'~1,
#       kat_stress == 'Sedang'~2,
#       kat_stress == 'Parah'~3,
#       kat_stress ==  'Sangat Parah'~4
#     ),
#     kat_stress = factor(kat_stress, levels = 0:4, ordered = T)
#   ) %>% 
#   dplyr::select(-c(stress, ikut_org))



df_hasil %>% 
  glimpse()

library(Hmisc)
library(MASS)


# proportional odds logistic regression -----------------------------------
m <- polr(kat_stress ~ (jabatan + lama_rapat + n_tugas + n_org)^2, hasil,  Hess = T, model = T)
summary(m)
step(m)


table(predict(m), hasil$kat_stress)
caret::confusionMatrix(predict(m), hasil$kat_stress)

m <- polr(kat_stress ~ jabatan, df_hasil, Hess = T)
summary(m)


# koefisien ---------------------------------------------------------------
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = round(p, 3)))

# odds ratio and ci
(ci <- confint(m))
confint.default(m) # CIs assuming normality
exp(cbind(odds_ratio = coef(m), ci))



# Uji serentak ------------------------------------------------------------
lmtest::lrtest(m)
anova(m, polr(kat_stress ~ 1, hasil, Hess = T))



# uji Goodnes of fit ------------------------------------------------------
# Ho : Model cocok dengan data
library(generalhoslem)

lipsitz.test(m)
logitgof(df_hasil$kat_stress, fitted(m), ord = TRUE)

pulkrob.chisq(m, c("jabatan"))
pulkrob.deviance(m, c("jabatan"))


# Pseudo R2 ---------------------------------------------------------------
# Tidak bisa menilai keakuratan model. 
# hanya untuk perbandingan saja biasanya
DescTools::PseudoR2(m, which = c('CoxSnell', 'Nagelkerke', 'McFadden'))



# Test of Parallel Lines --------------------------------------------------
# untuk proporional odds model, untuk menguji apakah
# koefisien slope sama untuk setiap kategori variabel respon

# Gagal Tolak Ho : Garis regresi pararel
brant::brant(m)


