library(tidyverse)
library(dm)
dm::my_conflict_prefer()

# -------------------------------------------------------------------------
# color
my_col <- c("#006D2C", "#20AC4B", "#7AC27F", "#A4D29F", "#CFE6CA")

# font
# https://www.fontsquirrel.com/fonts/poppins
windowsFonts(
  tnr = windowsFont("Times New Roman"),
  poppins = windowsFont("Poppins")
)

# 1.2 ---------------------------------------------------------------------
jrt_lvl <- c("Di Bawah Rata-rata (1-2)", "Sekitar Rata-rata (3-4)", "Di Atas Rata-rata (>4)")

dat1 <- tibble::tribble(
  ~kat, ~persentase,
  "Di Bawah Rata-rata (1-2)", 19.45392491,
  "Sekitar Rata-rata (3-4)", 52.90102389,
  "Di Atas Rata-rata (>4)", 27.64505119
) %>%
  mutate(
    kat = factor(kat, levels = jrt_lvl)
  )

dat1

my_family <- 'tnr'
dat1 %>%
  mutate(
    ypos = cumsum(persentase) - 0.45 * persentase
  ) %>%
  ggplot(aes(x = "", y = persentase, fill = rev(kat))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(
    aes(
      y = ypos, 
      # jangan lupa decimal.mark = ','
      label = scales::percent(persentase/100, decimal.mark = ',', accuracy = 0.01)
    ),
    color = "white", size = 4.5,
    fontface = 2,
    family = my_family
  ) +
  coord_polar("y", start = 0) +
  scale_fill_manual(
    values = rev(my_col[1:3]),
    labels = function(x) rev(str_wrap(x, 25))
  ) +
  # ganti font "tnr" dengan "poppins"
  theme_void(base_size = 13, base_family = my_family) +
  theme(
    plot.title = element_text(face = 2, vjust = 0),
    plot.subtitle = element_text(colour = "gray30", vjust = 0),
    legend.text = element_text(size = 12),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.7, "cm"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(0.1, "cm")
  ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(
    title = str_wrap("Persentase (%) Petani Pelaku Alih Fungsi Lahan Menurut Jumlah Anggota Rumah Tangga", 50),
    subtitle = "Kabupaten Bandung Barat dan Kabupaten Purwakarta",
    fill = NULL
  )

# ganti filename
ggsave(
  filename = "E:/Visualisasi/riset/revisi rah/1_2_tnr_koma.png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 500,
  scale = 0.6,
  bg = "white"
)




# 1.7 ---------------------------------------------------------------------

dat2 <- tibble::tribble(
  ~mekanisme, ~kat, ~persentase,
  "Melalui Transaksi Penjualan", "Di Bawah Rata-rata (1-2)", 21.3,
  "Melalui Transaksi Penjualan", "Sekitar Rata-rata (3-4)", 48.15,
  "Melalui Transaksi Penjualan", "Di Atas Rata-rata (>4)", 30.56,
  "Tidak Melalui Transaksi Penjualan", "Di Bawah Rata-rata (1-2)", 18.38,
  "Tidak Melalui Transaksi Penjualan", "Sekitar Rata-rata (3-4)", 55.68,
  "Tidak Melalui Transaksi Penjualan", "Di Atas Rata-rata (>4)", 25.95
)


dat2 <- dat2 %>%
  mutate(
    kat = factor(kat, levels = jrt_lvl)
  )


ggplot(dat2, aes(y = persentase, x = kat, fill = mekanisme, group = mekanisme)) +
  geom_bar(
    stat = "identity",
    position = position_dodge2(0.9)
  ) +
  geom_text(
    aes(y = persentase + 3, label = scales::percent(persentase / 100, decimal.mark = ',', accuracy = 0.01)),
    position = position_dodge(0.9),
    size = 3.9,
    family = my_family
  ) +
  scale_fill_manual(
    NULL,
    values =  c(my_col[2:1], my_col[2:1]),
    labels = function(x) str_wrap(x, 20)
  ) +
  scale_y_continuous(
    labels = NULL,
    expand = expansion(mult = c(0.01, 0.1))
  ) +
  scale_x_discrete(
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Kabupaten Bandung Barat dan Kabupaten Purwakarta",
    title = str_wrap("Persentase (%) Mekanisme Alih Fungsi Lahan Menurut Interval Jumlah Anggota Rumah Tangga", 70)
  ) +
  theme_minimal(base_family = "poppins") +
  theme(
    panel.spacing = unit(5, "lines"),
    axis.text.x = element_text(size = 10.5, margin = margin(b = 10, 0, 0, 0)),
    panel.grid = element_blank(),
    plot.title = element_text(face = 2, size = 13, vjust = 0),
    plot.subtitle = element_text(colour = "gray30", vjust = 0)
  )




ggsave(
  filename = "E:/Visualisasi/riset/revisi rah/1_7_poppins_koma.png",
  width = 7.5,
  height = 5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)




# peta --------------------------------------------------------------------


