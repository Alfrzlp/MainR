dat_jatim <- 
  readxl::read_xlsx('D:/tugas/dataPA_jatim.xlsx') %>% 
  dplyr::filter(!nmkab %in% kab_outlier) %>%
  mutate_at(vars(-nmkab), ~ as.numeric(scale(.x))) 

kab_outlier <- "Kabupaten Bangkalan"
glimpse(dat_jatim)

# dat_jatim %>% 
#   openxlsx::write.xlsx('D:/tugas/dataPA_jatim.xlsx')

# -------------------------------------------------------------------------
m <- lm(pm20 ~ hls + tpt20 + rk, data = dat_jatim)
m <- lm(pm20 ~ hls + tpt20, data = dat_jatim)

summary(m)


# ------------------------------- ------------------------------------------
msub <- lm(tpt20 ~ hls + rk, data = dat_jatim)
summary(msub)

round(coef(m), 3)

# -------------------------------------------------------------------------
library(broom)
get_hasil <- function(res){
  t(res[, 2]) %>% 
    as.data.frame() %>% 
    setNames(res$term)
}
res_m <- get_hasil(tidy(m))
res_msub <- get_hasil(tidy(msub))


# rls
res_m$tpt20 * res_msub$rls > res_m$rls
# pengaruh tdk langsung
res_m$tpt20 * res_msub$rls
# pengaruh langsung
res_m$rls^2
# total
res_m$tpt20 * res_msub$rls + res_m$rls^2



# amh
res_m$tpt20 * res_msub$amh > res_m$amh
# pengaruh tdk langsung
res_m$tpt20 * res_msub$amh
# pengaruh langsung
res_m$amh^2
# total
res_m$tpt20 * res_msub$amh + res_m$amh^2



# hls
res_m$tpt20 * res_msub$hls > res_m$hls
# pengaruh tdk langsung
res_m$tpt20 * res_msub$hls
# pengaruh langsung
res_m$hls^2
# total
(res_m$tpt20 * res_msub$hls + res_m$hls^2)*100


# rk
res_m$tpt20 * res_msub$rk > res_m$rk
res_m$tpt20 * res_msub$rk


# kp
res_m$tpt20 * res_msub$kp > res_m$kp
res_m$tpt20 * res_msub$kp + res_m$kp

# lpp
res_m$tpt20 * res_msub$lpp > res_m$lpp
res_m$tpt20 * res_msub$lpp + res_m$lpp


# tpt
res_m$pm20[1] * res_msub$tpt20[1] > res_m$tpt20[1]
res_m$pm20[1] * res_msub$tpt20[1] + res_m$tpt20[1]



# Heteroskedastisitas -----------------------------------------------------
library(lmtest)
bptest(m)
bptest(msub)
# gagal tolak homoskedastisitas


# linearitas --------------------------------------------------------------
# gagal tolak normal
shapiro.test(m$residuals)
shapiro.test(msub$residuals)

nortest::lillie.test(m$residuals)
nortest::lillie.test(msub$residuals)


# Multikolinearitas -------------------------------------------------------
library(car)
vif(m)
vif(msub)



# Uji Kelayakan Model -----------------------------------------------------
N <- nrow(dat_jatim)
d <- 1
mfull <- lm(pm20 ~ hls + tpt20 + rk, data = dat_jatim)
mtrim <- lm(pm20 ~ hls + tpt20, data = dat_jatim)
msub_r2 <- summary(msub)$r.squared

summary(mfull)$r.squared
summary(mtrim)$r.squared

# full model
R2M <- 1 - (1 - summary(mfull)$r.squared)*(1 - msub_r2)
M <- 1 - (1 - summary(mtrim)$r.squared)*(1 - msub_r2)
Q <- (1 - R2M)/(1 - M)
W <- -(N - d) * log(Q)
W

qchisq(0.05, df = 1, lower.tail = F)
# H0: Model alternatif memadai
ifelse(W > qchisq(0.1, df = 1, lower.tail = F), 'Tolak Ho', 'Gagal Tolak Ho')



# Viz ---------------------------------------------------------------------
ggplot(dat_jatim, aes(x = amh, y = pm20)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "maroon")








kab_outlier <- 
  readxl::read_xlsx('D:/tugas/dataPA_jatim.xlsx') %>% 
  slice(c(26)) %>% 
  pull(nmkab)
kab_outlier

kab_outlier <- c('Kabupaten Sampang', 'Kabupaten Bangkalan', "Kabupaten Sumenep")

library(olsrr)

# Outlier -----------------------------------------------------------------
# studentized residual
p <- ols_plot_resid_stud(m)
p$data
ols_plot_resid_stand(m)

# deleted studentized residual
# outlier y
ols_plot_resid_stud_fit(m)


# Amatan Berpengaruh ------------------------------------------------------
# cook distance
ols_plot_cooksd_bar(m)
ols_plot_cooksd_chart(m)

# DFFITS
ols_plot_dffits(m)

# DFBETAS
ols_plot_dfbetas(m)



# Outlier -----------------------------------------------------------------
# studentized residual
p <- ols_plot_resid_stud(msub)
p$data
ols_plot_resid_stand(msub)

# deleted studentized residual
# outlier y
ols_plot_resid_stud_fit(msub)


# Amatan Berpengaruh ------------------------------------------------------
# cook distance
ols_plot_cooksd_bar(msub)
ols_plot_cooksd_chart(msub)

# DFFITS
ols_plot_dffits(msub)

# DFBETAS
ols_plot_dfbetas(msub)



readxl::read_xlsx('D:/tugas/dataPA_jatim.xlsx') %>% 
  slice(c(26, 27, 29)) %>% 
  pull(nmkab)

# Model -------------------------------------------------------------------
%>% library(lavaan)

full_mod = '
tpt20 ~ hls + rk
pm20 ~ hls + tpt20
'

# mod.est <- sem(model = mod.id, data = scale(dat))
mod.est <- cfa(model = mod.id, data = dat_jatim)

summary(mod.est, rsquare = T)
summary(mod.est, fit.measures = TRUE)

fitmeasures(mod.est, c('GFI', 'cfi', 'NFI', 'NNFI','SRMR'))
# (lebih besar dari 0,90)
# (lebih besar dari 0,90)
# (lebih besar dari 0,90)
# (lebih besar dari 0,90)
# (kurang dari 0,05)
                    


# Path Diagram ------------------------------------------------------------
library(semPlot)

semPaths(
  object = mod.est,
  what = "path",
  whatLabels = "par",
  style = "ram",
  layout = "tree",
  residuals = T,
  rotation = 2,
  sizeMan = 7,
  sizeLat = 7,
  color = "lightgray",
  edge.label.cex = 1.2,
  label.cex = 1.3,
  filename = "E:/Visualisasi/tugas/path2"
)
