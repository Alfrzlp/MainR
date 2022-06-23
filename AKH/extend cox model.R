dat2 <- 
  survSplit(Surv(time, status) ~ jk + kat_umur + uk_tmr + std_tmr +
          n_simpul_tmr + kat_reshorA + kat_reshorB + terapi,
          data = dat, 
          cut = 300,
          # end = 'time',
          # event = 'status',
          episode = "tgroup",
          id = "id") %>% 
  mutate(
    # g1 = ifelse(time < 300, 1, 0),
    # g2 = ifelse(time >= 300, 1, 0),
    # resA_g1 = tgroup * g1,
    # resA_g2 = tgroup * g2
    resA = ifelse(kat_reshorA == 'rendah', 0, 1),
    resA = resA * tgroup
  ) %>% 
  arrange(id)


mcox_ex <- coxph(Surv(time, status) ~ kat_umur + jk + uk_tmr + std_tmr +
                n_simpul_tmr + kat_reshorB + terapi +
                 + cluster(id), data = dat2)
summary(mcox_ex)
AIC(mcox_ex)
AIC(mcox)
logLik(mcox)

cox.zph(mcox_ex)
