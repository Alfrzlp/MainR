# Read Data ---------------------------------------------------------------
x <- readxl::read_xlsx("D:/_Datasets/__Time Series/jagung.xlsx") %>%
  type_convert()

jagung <- x %>%
  `colnames<-`(c(
    "tahun", "luas", "produksi", "produktivitas",
    "kp", "krt", "pinp", "ekspor", "impor"
  )) %>%
  select(-c(4, 6, 8))

jagung %>%
  glimpse()

# Viz ---------------------------------------------------------------------
x %>%
  select(-c(4, 6, 8)) %>%
  pivot_longer(-1) %>%
  mutate(name = str_to_title(name)) %>%
  ggplot(aes(x = tahun, y = value)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  facet_wrap(~name, scales = "free")



# Uji Stasioneritas -------------------------------------------------------
library(urca)
library(tseries)

# lebih kecil Tolak Ho-Stasioner
ur.df(diff(jagung$impor)) %>%
  summary()
ur.df(diff(jagung$luas)) %>%
  summary()
ur.df(diff(jagung$produksi)) %>%
  summary()
ur.df(diff(jagung$kp)) %>%
  summary()
ur.df(diff(jagung$pinp)) %>%
  summary()


m1 <- lm(impor ~ ., jagung[, -1])
summary(m1)



# Uji kointegrasi ---------------------------------------------------------
et <- m1$residuals
ur.df(et) %>% summary()


# Model ECM ---------------------------------------------------------------
# Data diturunkan dan ditambahkan variabel et(-1)
data <-
  jagung[, -1] %>%
  apply(2, diff) %>%
  as.data.frame() %>%
  mutate(et = et[-length(et)])

mecm <- lm(impor ~ kp + pinp + luas + produksi + et, data)
summary(mecm)


# Uji Asumsi Klasik -------------------------------------------------------
# Normalitas
shapiro.test(mecm$residuals)

# Non autokorelasi
library(lmtest)
bgtest(mecm)

# Homoskedatisitas
bptest(mecm)

# Non autokorelasi
library(car)
vif(mecm)
