
loc <- 'D:/_Datasets/ID_2017_DHS_06022022_2341_163652/IDIR71SV/IDIR71FL.SAV'
dat <- foreign::read.spss(loc, to.data.frame = T)
glimpse(dat)



dat_final <- dat %>% 
  select(
    # S105, 
    # nmprov = V101,
    umur = V012,
    status = S1602,
    time = S1603,
    
    hubungan = V150,
    daerah = V025,
    pend = S108,
    pend2 = V106,
    proRokok = S1605,
    pernah_alkohol = S1607,
    pernah_mabuk = S1610,
    pernah_narkoba = S1614,
    pernah_pcr = S1702,
    kekayaan = V190,
    internet = V171A,
    f_tv = V159
  ) %>%
  dplyr::filter(
    # !is.na(time),
    !is.na(status),
    # time != 'Not Remember'
  ) %>% 
  mutate(
    time = ifelse(is.na(time) & status == 'No', umur, time)
  ) %>% 
  as_tibble()
  

na.omit() %>% 
  pull(status) %>%
  table()
  glimpse()
  dim()


colnames(dat)
dat$glimpse(dat[1:5,])

dat %>% 
  na.omit()


subdat <- dat[1:3, ]
label <- attr(subdat, "variable.labels")
class(label)


# -------------------------------------------------------------------------
kondef <- 
  data.frame(
    nama = colnames(subdat),
    label
  ) %>% 
  remove_rownames() %>% 
  as_tibble()
  

kondef %>% 
  dplyr::filter(str_detect(label, 'children')) %>% 
  as.data.frame()
  dplyr::filter(!str_detect(nama, '^B|^M'))

dat$V535

# -------------------------------------------------------------------------
dat_final <- dat %>% 
    select(
      # S105, 
      nmprov = V101,
      umur = V012,
      status = S1602,
      time = S1603,
      
      
      hubungan = V150,
      bekerja = V714,
      daerah = V025,
      
      pend = S108,
      pend2 = V106,
      
      proRokok = S1605,
      u_alkohol = S1608,
      
      pernah_alkohol = S1607,
      pernah_mabuk = S1610,
      pernah_narkoba = S1614,
      pernah_pcr = S1702,
      
      kekayaan = V190,
      internet = V171B,
      f_tv = V159,
      f_koran = V157,
      lc = V137
    ) %>%
    mutate(
      # jika status NA tapi ada waktunya
      status = ifelse(is.na(status) & !is.na(time), 2, status),
      # jika waktu awal merokok NA tapi status belum pernah rokok
      # maka waktu adalah umur (NO = 1)
      time = ifelse(is.na(time) & status == 1, umur, time),
      u_alkohol = as.numeric(levels(u_alkohol))[u_alkohol],
      palkohol = ifelse(u_alkohol <= time, 1, 0)
    ) %>% 
    dplyr::filter(
      !is.na(time)
    ) %>%
    drop_na(bekerja, proRokok, internet, palkohol) %>% 
    as_tibble()
    
  
pull(status) 
    table()

dat_final %>% 
  is.na() %>% 
  colSums()


dat_final <- dat_final %>% 
  mutate(
    pend3 = ifelse(pend %in% c("No education", "Primary", "Junior high school"),
                  'Dibawah SMA', 'Diatas SMA'),
    pend3 = factor(pend3, levels = c('Dibawah SMA', 'Diatas SMA'))
  )




# -------------------------------------------------------------------------
dat_final2 <- dat %>% 
  select(
    umur = V012,
    status = S1607,
    time = S1608,
    
    daerah = V025,
    pend = S108,
    # pend2 = V106,
    
    bekerja = V714,
    p_rokok = S1602,
    u_rokok = S1603,
    
    p_mabuk = S1610,
    proAlkohol = S1611,

    kekayaan = V190,
    internet = V171B,
    f_tv = V159,
    # f_koran = V157
  ) %>% 
  filter(
    # jika filter  time != "Don't know" maka NA tidak akan
    # dimasukkan 
    time != "Don't know" | is.na(time),
    !is.na(status),
    u_rokok != "Not Remember" | is.na(u_rokok)
  ) %>% 
  mutate(
    # jika status NA tapi ada waktunya
    status = ifelse(is.na(status) & !is.na(time), 2, status),
    # jika waktu awal minum NA tapi status belum pernah minum
    # maka waktu adalah umur (NO = 1)
    time = ifelse(is.na(time) & status == 1, umur, time),
    
    # jika usia pertama merokok < usia pertama alkohol 
    # maka pernah merokok sebelumnya
    u_rokok = as.numeric(levels(u_rokok))[u_rokok],
    pRokok = ifelse(u_rokok <= time, 'Yes', 'No'),
    pRokok = ifelse(p_rokok == 'Yes', 'Yes', 'No')
  ) %>% 
  as_tibble() %>% 
  select(-c(p_rokok, u_rokok)) %>% 
  drop_na(time, pRokok, internet) %>% 
  mutate(
    pend3 = ifelse(pend %in% c("No education", "Primary", "Junior high school", "Senior high school"),
                   'Dibawah SMA', 'Diatas SMA'),
    pend3 = factor(pend3, levels = c('Dibawah SMA', 'Diatas SMA')),
    f_tv2 = ifelse(f_tv == 'Not at all', 'Tidak Pernah', 'Minimal kurang dari 1 kali dlm seminggu'),
    internet2 = ifelse(
      internet %in% c('Not at all'),
      'tidak', 'ya'
    )
  ) %>% 
  glimpse()


dat_final2 <- dat_final2 %>% 
  mutate(
    daerah = factor(daerah, labels = c('Perkotaan', 'Perdesaan')),
    kekayaan = factor(kekayaan, labels = c('Sangat Miskin', 'Miskin', 'Tidak Miskin', 'Kaya', 'Sangat Kaya')),
  )

ggsurvplot(
  survfit(Surv(time, status) ~ daerah, data = dat_final2),
  conf.int = F,
  ylim = c(0.85, 1),
  pval = T,
  pval.coord = c(1, 0.9)
)


