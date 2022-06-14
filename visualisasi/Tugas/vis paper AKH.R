df_final <- dat_final2

df_final %>% 
  count(status) %>% 
  mutate(p = n/sum(n))

ggplot(df_final) +
  geom_bar(aes(x = umur))



# KM ----------------------------------------------------------------------
ggsurvplot(
  survfit(Surv(time, status) ~ pend3, data = dat_final2),
  conf.int = T,
  ylim = c(0.9, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, 0.935),
  pval.coord = c(1, 0.925),
  legend.labs = c('≤ SMA', '> SMA')
)

x <- 0.3
ggsurvplot(
  survfit(Surv(time, status) ~ proAlkohol, data = dat_final2),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.18),
  pval.coord = c(1, x + 0.1)
)

x <- 0.7
ggsurvplot(
  survfit(Surv(time, status) ~ pRokok, data = dat_final2),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.135),
  pval.coord = c(1, x + 0.1),
  legend.labs = c('Belum Pernah Merokok','Pernah Merokok')
)

x <- 0.9
ggsurvplot(
  survfit(Surv(time, status) ~ daerah, data = dat_final2),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.035),
  pval.coord = c(1, x + 0.025)
)


x <- 0.9
ggsurvplot(
  survfit(Surv(time, status) ~ kekayaan, data = dat_final2),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.035),
  pval.coord = c(1, x + 0.025),
  legend.labs = c('Sangat Miskin', 'Miskin', 'Tidak Miskin', 'Kaya', 'Sangat Kaya')
)


x <- 0.85
ggsurvplot(
  survfit(Surv(time, status) ~ internet, data = dat_final2),
  conf.int = T,
  ylim = c(x, 1),
  pval = T,
  pval.method = T,
  pval.method.coord = c(1, x + 0.04),
  pval.coord = c(1, x + 0.025),
  legend.labs = c('Tidak Pernah', 'Minimal \n1X sebulan', 'Minimal \n1X Seminggu', 'Hampir \nSetiap Hari')
)


ggsave(
  filename = 'E:/Visualisasi/tugas/paper AKH/km_internet.png',
  dpi = 500,
  width = 7,
  height = 4.5,
  scale = 0.9,
  bg = 'white'
)






# Analisis deskriptif -----------------------------------------------------
dat_final2 %>% 
  ggplot(aes(x = umur)) +
  geom_bar(
    aes(fill = factor(status)),
    position = 'fill'
  ) +
  geom_text(
    data = datku,
    fontface = 1.5,
    size = 3,
    aes(
      y = p + 0.03 * sign(p),
      label = scales::percent(p)
    )
  ) +
  scale_x_continuous(
    breaks = 15:24
  ) +
  scale_y_continuous(
    'Persentase',
    labels = scales::percent
  ) +
  scale_fill_manual(
    str_wrap('Status Minum Minuman Beralkohol', 15),
    labels = c('Belum Pernah', 'Pernah'),
    values = c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")[1:2]
  ) 
  

ggsave(
    filename = 'E:/Visualisasi/tugas/paper AKH/bar_descr.png',
    dpi = 500,
    width = 8,
    height = 4,
    scale = 0.9,
    bg = 'white'
  )
  




# -------------------------------------------------------------------------
datku <- filter(dat_final2) %>% 
  group_by(umur) %>% 
  count(status)  %>% 
  mutate(p = n/sum(n)) %>% 
  filter(status == 2) %>% 
  ungroup()


# Bar Chart ---------------------------------------------------------------
dat_final2 %>% 
  group_by(status) %>% 
  count(proAlkohol) %>% 
  mutate(p = n/sum(n)) %>% 
  
  ggplot(aes(x = factor(status), y = p,
             color = proAlkohol, fill = proAlkohol)) +
  geom_col(
    position = position_stack(reverse = T)) +
  geom_text(
    aes(label = scales::percent(p)),
    position = position_fill(vjust = 0.5, reverse = T),
    size = 4, fontface = 2
  ) +
  scale_x_discrete(
    'Minum Minuman Beralkohol',
    labels = c('Belum Pernah', 'Pernah')
  ) +
  scale_fill_manual(
    NULL,
    values = c('steelblue4', 'skyblue')
    # values = my_col_green[c(2, 4)]
  ) +
  scale_color_manual(
    values = c('white', 'black')
  ) +
  guides(color = 'none') +
  scale_y_continuous(
    'Persentase',
    breaks = NULL,
    labels = NULL
  )

ggsave(
  filename = 'E:/Visualisasi/tugas/paper AKH/bar_pend.png',
  dpi = 500,
  width = 6,
  height = 4,
  scale = 0.7,
  bg = 'white'
)



# comparison --------------------------------------------------------------
lnorm <- survreg(Surv(time, status) ~ 1,
                     data = dat_final2, dist = 'lognormal')
llog <- survreg(Surv(time, status) ~ 1,
                    data = dat_final2, dist = 'loglog')
wei <- survreg(Surv(time, status) ~ 1,
                   data = dat_final2, dist = 'weibull')
expo <- survreg(Surv(time, status) ~ 1,
                   data = dat_final2, dist = 'exp')


str(expo)
plot(expo, ylim = c(0.9, 1))



all_model <- list(expo, wei, llog, lnorm) 
nama <- unlist(sapply(all_model, function(x) x$dist))

data.frame(
  model = nama,
  aic = sapply(all_model, AIC)
) %>% 
  arrange(aic) 
  dm::separator_convert(aic, '\\.', ',',  to_numeric = F) %>% 
  clipr::write_clip()


    
ks.test(dat_final2$umur, 'pweibull', 0.503, 1)


library(survminer)
ggsurvplot(lnorm, data = dat_final2)
