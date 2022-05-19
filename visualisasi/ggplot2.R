perempuan <- c(
  180, 120, 180, 360, 240, 120, 180, 120, 240, 170, 150, 120, 180, 180, 150,
  200, 150, 180, 150, 180, 120, 60, 120, 180, 180, 90, 240, 180, 115, 120
)

laki <- c(
  90, 120, 30, 90, 200, 90, 45, 30, 120, 75, 150, 120, 60, 240, 300, 240, 60, 120,
  60, 30, 30, 230, 120, 95, 150, 0, 200, 120, 120, 180
)

df <- data.frame(c(perempuan, laki), rep(c("Wanita", "Laki-Laki"), each = 30))
colnames(df) <- c("Lama_Belajar", "Jenis_Kelamin")
df

pendidikan_Rendah <- c(12, 33, 18, 13, 15, 2, 23, 17, 9, 11, 5, 7)
Buta_Huruf <- c(16, 25, 29, 4, 23, 7, 3, 27, 14, 14, 9, 6)
Kemiskinan <- c(26, 19, 7, 30, 22, 10, 8, 29, 28, 12, 4, 21)

masyarakat <- data.frame(pendidikan_Rendah, Buta_Huruf, Kemiskinan)
masyarakat
# ==============================================================================
# pie cart
library(ggplot2)
library(dplyr)
pria <- df %>%
  group_by(Lama_Belajar, Jenis_Kelamin) %>%
  count() %>%
  filter(Jenis_Kelamin == "Laki-Laki")
head(pria)

ggplot(pria, aes(x = "", y = n, fill = Lama_Belajar)) +
  geom_bar(stat = "identity", width = 1, color = "white") + # white garis
  coord_polar("y", start = 0) +
  ggtitle("Lama Belajar Mahasiswa")

pie(pria$n,
  col = rainbow(nrow(pria), start = 0.1, end = 0.8),
  clockwise = T
)
# ===========================================================================
# scater plot
ggplot(iris) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, col = Species))

#
penduduk <- readxl::read_xls("D:/Datasets/penduduk_indonesia.xls", sheet = "Sheet1")
penduduk

library(janitor)
penduduk <- clean_names(penduduk)
attach(penduduk)
penduduk

library(ggplot2)
# ===========================================================================
# piramida penduduk
ggplot(penduduk, aes(y = reorder(kelompok_umur, 1:20))) +
  geom_bar(aes(x = -laki_laki, fill = "blue"), stat = "identity") +
  geom_bar(aes(x = perempuan, fill = "red"), stat = "identity") +
  scale_x_continuous(
    breaks = seq(-1e+07, 1e+07, 5e+06),
    labels = c("10 juta", "5 juta", "0", "5 juta", "10 juta")
  ) +
  labs(
    title = "Piramida Penduduk Indonesia Tahun 2010",
    subtitle = "Sensus Penduduk 2010",
    x = "Populasi", y = "Kelompok Umur", fill = "Jenis Kelamin"
  ) +
  scale_fill_discrete(labels = c("Perempuan", "Laki-laki"))

mtcars
library(dplyr)
data <- data.frame(mobil = rownames(mtcars), mpg_z = scale(mtcars$mpg))
data <- data %>%
  mutate(posisi = if_else(mpg_z < 0, "bawah", "atas")) %>%
  arrange(-mpg_z)
data

# ===========================================================================
# lolipop
ggplot(data) +
  geom_point(aes(x = mpg_z, y = reorder(mobil, 32:1), col = posisi, size = 6)) +
  geom_segment(aes(
    x = 0, y = reorder(mobil, 32:1),
    xend = mpg_z, yend = reorder(mobil, 32:1), col = posisi
  )) +
  geom_text(aes(
    x = mpg_z + 0.3, y = reorder(mobil, 32:1),
    label = round(mpg_z, 3), size = 6
  )) +
  theme(legend.position = "none") +
  labs(y = "Jenis Mobil")


# ===========================================================================
# bar cart
ggplot(data, aes(x = mpg_z, y = reorder(mobil, 32:1))) +
  geom_col(aes(fill = posisi)) +
  theme(legend.position = "none") +
  labs(y = "Jenis Mobil")

# ===========================================================================
# titik
library(gapminder)
gapminder %>%
  filter(year == 2007) %>%
  group_by(country) %>%
  summarise(
    lifeExp = mean(lifeExp), gdpPercap = mean(gdpPercap),
    pop = mean(pop), continent
  ) %>%
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp, col = continent, size = pop))


# ===========================================================================
# set warna
df <- data.frame(
  pdb = c(17475, 16298, 12855, 11347),
  Negara = c(
    "Jepang \n tahun 1970",
    "Singapura \n tahun 1991",
    "Hongkong \n tahun 1982",
    "Korea \n tahun 2000"
  )
)

ggplot(df) +
  geom_col(aes(x = reorder(Negara, -pdb), y = pdb, fill = Negara)) +
  scale_fill_viridis_d(direction = -1) +
  theme_test() +
  geom_text(aes(x = Negara, y = pdb + 1500, label = paste("PDB", pdb / 1000)), col = "white", size = 5) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(colour = "white", size = 16),
    axis.text.y = element_blank(),
    axis.line = element_line(colour = "white", size = 2),
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.background = element_rect(fill = "black")
  )
# ===========================================================================
# animasi
library(gapminder)
library(ggplot2)
library(gganimate)
library(gifski)

myplot <- ggplot(gapminder) +
  geom_point(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  scale_x_log10() +
  theme_bw() +
  labs(title = "Year: {frame_time}", x = "GDP Perkapita", y = "Life Expectancy") +
  transition_time(year) +
  ease_aes("linear")

# width=200, height=200 bisa diatur juga
animate(myplot,
  duration = 8, fps = 20,
  renderer = gifski_renderer()
)
anim_save("gganimate1.gif")

# =================================================================
myplot2 <- ggplot(gapminder) +
  geom_point(aes(x = gdpPercap, y = lifeExp, size = pop, color = country),
    alpha = 0.7, show.legend = F
  ) +
  scale_color_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~continent) +
  scale_x_log10() +
  # gganimate
  labs(title = "Year: {frame_time}", x = "GDP Perkapita", y = "Life Expectancy") +
  transition_time(year) +
  ease_aes("linear")

animate(myplot2,
  duration = 8, fps = 20,
  renderer = gifski_renderer()
)

anim_save("facet.gif")


library(coronavirus)
covid_indo <- coronavirus %>%
  filter(country == "Indonesia") %>%
  select(date, type, cases)

library(ggplot2)
library(gganimate)

grafik_covid <- ggplot(covid_indo, aes(x = date, y = cases, col = type)) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = c("yellow", "red", "limegreen")) +
  labs(x = "Bulan", y = "Kasus") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "white"),
    axis.text = element_text(colour = "white"),
    axis.title = element_text(colour = "cyan"),
    title = element_text(colour = "gold"),
    panel.background = element_rect(colour = "black", fill = "black"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(colour = "white", size = 11),
    legend.key = element_blank(),
    legend.key.size = unit(3, "lines"),
    legend.key.height = unit(0, "cm")
  ) +
  transition_reveal(date)

# , title = "Covid-19 di Indonesia"
# view_follow(fixed_y = T)
animate(grafik_covid,
  duration = 19, fps = 30, width = 800,
  renderer = gifski_renderer()
)

anim_save("covid___.gif")

# ========================================================================
a <- data.frame(group = c("a", "b", "c"), values = c(3, 2, 4), frame = rep("a", 3))
b <- data.frame(group = c("a", "b", "c"), values = c(5, 3, 7), frame = rep("b", 3))
data2 <- rbind(a, b)

ggplot(data2) +
  geom_bar(aes(x = group, y = values, fill = group), stat = "identity") +
  theme_bw() +
  transition_states(frame)
# ===========================================================================

df <- readxl::read_xlsx("D:/Datasets/proyeksi.xlsx", sheet = 1)
df <- df %>% mutate(jenis_kelamin = rep(c("Laki_laki", "Perempuan"), each = 16))

df <- df %>% tidyr::pivot_longer(cols = 2:27, names_to = "tahun", values_to = "jumlah")

data <- cbind(df[1:416, ], df[417:832, ])
data <- data[, c(1, 3, 4, 8)]
colnames(data)[3:4] <- c("Laki_Laki", "Perempuan")

data <- data %>% arrange(tahun)
data
color <- c("pria" = "steelblue4", "wanita" = "lightblue3")

pddk <- ggplot(data, aes(y = reorder(UMUR, 1:416))) +
  geom_bar(aes(x = -Laki_Laki, fill = "pria"), stat = "identity") +
  geom_bar(aes(x = Perempuan, fill = "wanita"), stat = "identity") +
  labs(
    title = "Tahun: {closest_state}", y = "Kelompok Umur", x = "Jumlah Penduduk",
    fill = "Jenis Kelamin"
  ) +
  scale_x_continuous(
    breaks = c(-2e+05, 0e+00, 2e+05),
    labels = c("2 juta", "0", "2 juta")
  ) +
  scale_fill_manual(values = color) +
  theme_bw() +
  transition_states(tahun)
# states closest, next, previous

animate(pddk,
  fps = 30, duration = 10,
  renderer = gifski_renderer()
)

anim_save("proyeksi_.gif")
