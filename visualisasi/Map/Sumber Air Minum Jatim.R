library(tidyverse)

df <- readxl::read_xls("C:/Users/Ridson Alfarizal/Downloads/Indo_29_4976153.xls") %>%
  drop_na(1) %>%
  mutate_all(function(x) {
    ifelse(x == "-", 0, x)
  })

df <- df %>%
  summarise_all(funs(sum(!is.na(.)))) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("sam") %>%
  filter(!sam %in% c("Kabupaten/Kota", "Jumlah"))


ggplot(df) +
  geom_col(aes(y = reorder(sam, V1), x = V1, fill = V1)) +
  geom_text(aes(y = reorder(sam, V1), x = V1 + 2, label = V1)) +
  labs(
    x = "Jumlah Kabupaten/Kota", y = "Sumber Air Minum",
    title = "Distribusi Sumber air minum di Provinsi Jawa Timur Tahun 2019",
    subtitle = "Sumber : jatim.bps.go.id"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


df <- df %>%
  type_convert() %>%
  pivot_longer(2:12) %>%
  filter(name != "Jumlah")

my_caption <- expression(italic("Sumber Data : bps.go.id"))
df %>%
  filter(name != "Jumlah") %>%
  mutate(order = rep(1:10, length = 380)) %>%
  group_by(`Kabupaten/Kota`) %>%
  mutate(rank = sum(value[1:4])) %>%
  # filter(str_detect(`Kabupaten/Kota`, 'Kota')) %>%
  ggplot(aes(fill = reorder(name, order), x = value, y = reorder(`Kabupaten/Kota`, rank))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    fill = "Sumber Air Minum", x = "Presentase", y = "Kabupaten / Kota",
    title = "Distribusi Sumber air minum di Provinsi Jawa Timur Per Kabupaten/Kota Tahun 2019",
    caption = my_caption
  ) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    legend.key.height = unit(0.05, "cm")
  )
# 3 2
ggsave("D:/tk8.png", width = 25, height = 15, units = "cm")
"https://jatim.bps.go.id/statictable/2020/07/30/2093/distribusi-persentase-rumah-tangga-di-provinsi-jawa-timur-menurut-kabupaten-kota-dan-sumber-air-minum-2019.html"
