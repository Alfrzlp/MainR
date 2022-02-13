library(tidyverse)
library(car)

susenas <- read.csv("D:/Datasets/susenasUAS.csv")
head(susenas)

unique(susenas$sektor_pekerjaan)

# No 1-----------------------
# H0 : rata2 pengeluaran art industri - rata2 pengeluaran art pertanian <= 0
# H1 : rata2 pengeluaran art industri - rata2 pengeluaran art pertanian > 0
datasplit <- split(susenas$pengeluaran, susenas$sektor_pekerjaan)
industri <- datasplit$industri
pertanian <- datasplit$pertanian

shapiro.test(industri)
shapiro.test(pertanian)

bartlett.test(pengeluaran ~ sektor_pekerjaan, susenas)

t.test(industri, pertanian, alternative = "g", var.equal = T)
# gagal tolak Ho, pada taraf uji 5% belum cukup bukti untuk menyatakan bahwa rata2 pengeluaran
# art sektor pekerjaan industri lebih besar dari pada sektor pertanian



# No 2-------------------------
by(susenas$pengeluaran, susenas$status_pekerjaan, shapiro.test)
# normal jika alpha 0.01

leveneTest(pengeluaran ~ status_pekerjaan, susenas)
# p value > alpha maka gagal tolak Ho. maka asusmsi asumsi varians sama terpenuhu

anova <- aov(pengeluaran ~ status_pekerjaan, susenas)
summary(anova)
# p value > alpha maka gagal tolak Ho.
# artinya rata2 pengeluaran sama antar status pekerjaan


# No 3 -------------------------
unique(susenas$jenis_lantai)

tabel <- table(susenas$status_daerah, susenas$jenis_lantai)
chisq.test(tabel)









# 2018-2019 ------------------------------------------------------------------
# 2019-2020 ------------------------------------------------------------------
library(tidyverse)
library(nycflights13)

str(flights)
str(airlines)
str(planes)
str(weather)

data <- left_join(flights, airlines, by = "carrier")

# ambil tahun pembuatan pesawat dari data planes
planes <- planes %>%
  select(year, tailnum) %>%
  rename(year_manufactur = year)

data <- data %>% left_join(planes, by = "tailnum")

head(data, 10)
glimpse(data)

# dep_delay ada - dan +, + karena lebih cepat
my_caption <- expression(italic("Sumber Data : nycflights13"))
data %>%
  group_by(name) %>%
  summarise(average_depdelay = mean(abs(dep_delay), na.rm = T)) %>%
  arrange(average_depdelay) %>%
  top_n(10) %>%
  ggplot() +
  geom_col(aes(x = average_depdelay, y = reorder(name, average_depdelay)),
    fill = "steelblue"
  ) +
  labs(
    y = "Nama Maskapai", x = "rata-rata keterlambatan (menit)",
    title = "10 maskapai Rata-rata dengan keterlambatan terlama",
    caption = my_caption
  )


# B---------
sblm2000 <- data %>%
  filter(year_manufactur < 2000) %>%
  select(distance) %>%
  pull()
sblm2000

sdh2000 <- data %>%
  filter(year_manufactur >= 2000) %>%
  select(distance) %>%
  pull()
sdh2000

lillie.test(sblm2000)
lillie.test(sdh2000)

var.test(sblm2000, sdh2000)

t.test(sblm2000, sdh2000, var.equal = F)




# D---------
glimpse(data)
data <- data %>%
  mutate(musim = case_when(
    month %in% c(10:12, 1) ~ "winter",
    month %in% c(2:5) ~ "spring",
    month %in% c(6:9) ~ "summer"
  ))
glimpse(data)



my_caption <- expression(italic("Sumber Data : nycflights13"))
data %>%
  mutate(
    dep_delay = abs(dep_delay),
    arr_delay = abs(arr_delay)
  ) %>%
  select(dep_delay, arr_delay, carrier, name) %>%
  group_by(carrier) %>%
  summarise(
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    mean_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  pivot_longer(2:3, names_to = "variable") %>%
  ggplot() +
  geom_col(aes(
    x = reorder(carrier, value),
    y = value,
    fill = variable
  ),
  position = "dodge"
  ) +
  labs(
    title = "Rata-rata delay (menit)\nKeberangkatan dan Kedatangan Bedasarkan Maskapai",
    fill = "",
    x = "",
    y = "",
    caption = my_caption
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(
    labels = c("Delay Keberangkatan", "Delay Kedatangan"),
    palette = "Set2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.margin = margin(c(20, 20, 20, 10)),
    legend.box.margin = margin(-10, 0, 0, 0)
  )



my_caption <- expression(italic("Sumber Data : nycflights13"))
data %>%
  filter(arr_delay > dep_delay) %>%
  mutate(tambahan_diudara = arr_delay - dep_delay) %>%
  select(tambahan_diudara, carrier) %>%
  group_by(carrier) %>%
  summarise(jumlah = mean(tambahan_diudara, na.rm = TRUE)) %>%
  arrange(jumlah) %>%
  ggplot(aes(
    x = reorder(carrier, jumlah),
    y = jumlah
  )) +
  geom_col(fill = "#fc8d62") +
  geom_text(aes(label = round(jumlah, 1)), vjust = -0.7, size = 3) +
  labs(
    title = "Rata-rata Tambahan Waktu (menit)\ndi Udara Bedasarkan Maskapai",
    x = "Maskapai",
    y = "",
    caption = my_caption
  ) +
  scale_y_continuous(expand = c(0, NA), limits = c(0, 23)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.margin = margin(c(20, 20, 20, 10)),
    legend.box.margin = margin(-10, 0, 0, 0),
    axis.text.y = element_blank()
  )
