
# polygon -----------------------------------------------------------------
indo <- st_read("D:/_Datasets/gadm36_IDN_shp/gadm36_IDN_2.shp")
head(indo)

jatim <- indo %>% filter(NAME_1 == "Jawa Timur")


# Data --------------------------------------------------------------------
viirs <- raster("E:/Citra/apg/NOAA_VIIRS_2020_jawa.tif")
s5p_no2 <- raster("E:/Citra/apg/S5P_N02_2020_jawa.tif")
s5p_co <- raster("E:/Citra/apg/S5P_CO_2020_jawa.tif")

nlayers(s5p_no2)
nlayers(s5p_co)


co <- sapply(1:nrow(jatim), function(i) {
  x <- crop(s5p_co, jatim[i, ])
  x <- mask(x, jatim[i, ])
  return(cellStats(x, stat = "mean", na.rm = TRUE))
})
no2 <- sapply(1:nrow(jatim), function(i) {
  x <- crop(s5p_no2, jatim[i, ])
  x <- mask(x, jatim[i, ])
  return(cellStats(x, stat = "mean", na.rm = TRUE))
})
ntl <- sapply(1:nrow(jatim), function(i) {
  x <- crop(viirs, jatim[i, ])
  x <- mask(x, jatim[i, ])
  return(cellStats(x, stat = "mean", na.rm = TRUE))
})



# Data Remote Sensing -----------------------------------------------------
df_rs <-
  data.frame(
    kab = jatim$NAME_2,
    ntl = viirs,
    co, no2
  ) %>%
  mutate(
    kab = ifelse(kab %in% c("Batu", "Surabaya"), paste("Kota", kab), kab)
  )


# Data BPS ----------------------------------------------------------------
df_bps <- readxl::read_xlsx("D:/Downloads/apg.xlsx")
df_bps <- df_bps %>%
  mutate(
    kab = str_remove_all(kab, "Kabupaten ")
  ) %>%
  type_convert()



# Join --------------------------------------------------------------------
get_diff(df_bps$kab, df_rs$kab)


df_final <- df_bps %>% 
  left_join(df_rs, by = 'kab')
head(df_final)

cor(df_final[, -1])




# Korelasi Kanonik --------------------------------------------------------
X_rs <- df_final[ ,c('ntl', 'co', 'no2')] %>% scale()
X_bps <- df_final[ ,c('kp', 'jk', 'kayu')] %>% scale()

res.cc <- candisc::cancor(X_rs, X_bps)
summary(res.cc)
res.cc

#  Persentase Rumah Tangga Bakar Utama untuk Memasak adalah kayu



# Multiavriate Normality --------------------------------------------------
mvnormtest::mshapiro.test(t(X_rs))
mvnormtest::mshapiro.test(t(X_bps))

# X_rs <- df_rs %>%
#   mutate(
#     ntl = ntl %>% log(),
#     no2 = no2 %>% log()
#   ) %>% 
#   select(-kab)

MVN::mvn(X_rs, mvnTest = 'mardia', multivariateOutlierMethod = 'adj')
MVN::mvn(X_bps, mvnTest = 'mardia', multivariateOutlierMethod = 'adj')




# non-multikolinearitas ---------------------------------------------------
car::vif(lm(1:38 ~ ntl + co + no2, data = X_rs)) %>% round(3)
car::vif(lm(1:38 ~ kp + jk + kayu, data = X_bps)) %>% round(3)


# Uji Simultan Korelasi Kanonik -------------------------------------------
n <- nrow(df_final)
p <- ncol(X_rs)
q <- ncol(X_bps)
-((n - 1) - (p + q + 1)/2) * log(prod(1 - res.cc$cancor^2))

qchisq(0.05, p * q, lower.tail = F)

# pvalue
pchisq(
  -((n - 1) - (p + q + 1)/2) * log(prod(1 - res.cc$cancor^2)),
  p * q,
  lower.tail = F
)

CCP::p.asym(res.cc$cancor, n, p, q) %>% 
  clipr::write_clip()


# Uji Parsial --------------------------------------------------------------
m <- min(p, q)
l <- 1
-((n - 1) - (p + q + 1)/2) * log(prod(1 - res.cc$cancor[l:m]^2))
qchisq(0.05, (p - l) * (q - l), lower.tail = F)


l <- 2
-((n - 1) - (p + q + 1)/2) * log(prod(1 - res.cc$cancor[l:m]^2))
qchisq(0.05, (p - l) * (q - l), lower.tail = F)
pchisq(
  -((n - 1) - (p + q + 1)/2) * log(prod(1 - res.cc$cancor[l:m]^2)),
  (p - l) * (q - l),
  lower.tail = F
)


l <- 3
-((n - 1) - (p + q + 1)/2) * log(prod(1 - res.cc$cancor[l:m]^2))
qchisq(0.05, (p - l) * (q - l), lower.tail = F)




library(clipr)
# interpretasi ------------------------------------------------------------
# bobot
res.cc$coef
rbind(
  res.cc$coef$X,
  res.cc$coef$Y
) %>% 
  clipr::write_clip()

# beban
res.cc$structure

# beban kanonik
rbind(
  res.cc$structure$X.xscores,
  res.cc$structure$Y.yscores
) %>% 
  write_clip()

# beban silang
rbind(
  res.cc$structure$X.yscores,
  res.cc$structure$Y.xscores
) %>% 
  write_clip()






# viz ---------------------------------------------------------------------
test_spdf <- as(viirs, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

plot(viirs,
     box = FALSE,
     axes = FALSE,
     col = grey(1:100/100))

ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=value)) +
  scale_fill_brewer()
