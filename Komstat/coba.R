library(ggplot2)
# ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +<GEOM_FUNCTION>()
diamonds_1 <-
  ggplot(
    data = diamonds,
    mapping = aes(x = carat, y = price, colour = clarity)
  ) +
  geom_point()
summary(diamonds_1)

# ggplot(data = <DATA>) + <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
diamonds_2 <-
  ggplot(diamonds) +
  geom_point(aes(carat, price, colour = clarity))

summary(diamonds_c2)

# ggplot() +<GEOM_FUNCTION>(data = <DATA>, mapping = aes(<MAPPINGS>)
diamonds_3 <-
  ggplot() +
  geom_point(diamonds, aes(carat, price, colour = clarity))

summary(diamonds_3)

#--------------------------------------------
library(dplyr)
glimpse(storms)

View(storms)
storms %>%
  select(year, month, wind, pressure) %>%
  filter(between(year, 2000, 2015)) %>%
  mutate(month = factor(month.name[month], levels = month.name)) %>%
  group_by(month) %>%
  summarise(
    avg_wind = mean(wind),
    avg_pressure = mean(pressure)
  )

#------------------------------------------------------------------------------
library(janitor)
library(readxl)

indodapoer <- read.csv("D:/Datasets/indodapoer.csv")
head(colnames(indodapoer), 15)
indodapoer <- clean_names(indodapoer)
head(colnames(indodapoer), 15)

View(indodapoer)

library(stringr)
library(dplyr)

pdrb_pjawa <-
  indodapoer %>%
  filter(
    area_name %in% c(
      "Banten, Prop.",
      "DKI Jakarta, Prop.",
      "Jawa Barat, Prop.",
      "Jawa Tengah, Prop.",
      "DI Yogyakarta, Prop.",
      "Jawa Timur, Prop."
    )
  ) %>%
  transmute(
    provinsi = str_remove(area_name, ", Prop."),
    tahun = year,
    pdrb_nonmigas = total_gdp_excluding_oil_and_gas_in_idr_million_constant_price
  ) %>%
  filter(!is.na(pdrb_nonmigas))

pdrb_pjawa <- glimpse(pdrb_pjawa)
View(pdrb_pjawa)

library(ggplot2)
library(forecast)

ggplot %>%
  mutate(
    provinsi = fct_reorder2(provinsi, tahun, pdrb_nonmigas)
  ) %>%
  ggplot(aes(tahun, pdrb_nonmigas, colour = provinsi)) +
  geom_line()

library(ggplot2)
library(dplyr)
library(directlabels)

pdrb_pjawa %>%
  ggplot(aes(tahun, pdrb_nonmigas)) +
  geom_line(aes(colour = provinsi), show.legend = F) +
  geom_dl(
    aes(label = provinsi),
    method = "last.points",
    position = position_nudge(x = 0.3) # agar teks tidak berhimpitan dengan garis
  )

pdrb_pjawa %>%
  ggplot(aes(tahun, pdrb_nonmigas / 1e6)) +
  geom_line(aes(colour = provinsi), show.legend = FALSE) +
  geom_dl(
    aes(label = provinsi),
    method = "last.points",
    position = position_nudge(x = 0.3) # agar teks tidak berhimpitan dengan garis
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "PDRB Non-Migas di Pulau Jawa Hingga Tahun 2011",
    subtitle = "PDRB atas dasar harga konstan, dalam satuan triliun",
    caption = "Data INDO-DAPOER, The World Bank"
  ) +
  coord_cartesian(clip = "off")
