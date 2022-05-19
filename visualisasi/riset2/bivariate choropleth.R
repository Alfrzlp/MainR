library(tidyverse)
library(sf)
library(ggspatial)
library(cowplot)


my_col <- c("#FEE6CE", "#FDAE6B", "#E6550D", "#E5F5E0", "#A1D99B", "#31A354", "#DEEBF7", "#9ECAE1", "#3182BD")


# Data --------------------------------------------------------------------
peta_gab <- st_read('E:/CitraRiset2/shp_kbb_pwk/kbb_pwk.shp')
head(peta_gab)

dat <- 
  openxlsx::read.xlsx('D:/Downloads/Data Tujuan 2.xlsx', sheet = 'Sheet5') %>% 
  janitor::clean_names() # rapihin nama kolom
  
glimpse(dat)



# Pilih kolom yang penting. select bisa sambil rename misal kec = r103n
dat <- dat %>% 
  dplyr::select(c(
    kec = r103n, y1,
    starts_with(c('kepadat', 'jumlah', 'luas'))
  ))
dat



# join data
peta_gab <- peta_gab %>% 
  left_join(dat, by = c('nmkec' = 'kec'))

glimpse(peta_gab)





# Viz ---------------------------------------------------------------------
library(biscale)
data <- 
  bi_class(peta_gab, x = kepadatan_penduduk, y = y1, style = "jenks", dim = 3, keep_factors = T)




# "Brown", "DkBlue", "DkCyan", "DkViolet", or "GrPink"
map1 <- data %>% 
  mutate(
    bi_class = factor(bi_class, levels = paste0(rep(1:3, each = 3), '-', 1:3))
  ) %>% 
  ggplot() +
  # annotation_map_tile(
  #   'cartolight',
  #   zoom = 11,
  #   cachedir = 'cahcedir_base'
  # ) +
  geom_sf(
    aes(fill = bi_class),
    color = "white", show.legend = F, size = 0.7
  ) +
  # bi_scale_fill(pal = "DkCyan", dim = 3, ) +
  scale_fill_manual(
    values = stevens.greenblue()
  ) +
  labs(
    title = str_wrap('Kepadatan Penduduk dan Laju Alih Fungsi Lahan 2013-2021', 40),
    subtitle = 'Kabupaten Bandung Barat dan Purwakarta'
  ) +
  theme_void(base_family = 'Arial', base_size = 13) +
  theme(
    plot.title = element_text(face = 'bold'),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(clip = 'off')

map1



# Legend ------------------------------------------------------------------
legend1 <- 
  bi_legend(
    pal = "DkCyan",
    dim = 3, 
    xlab = str_wrap("Kepadatan Penduduk ", 9),
    ylab = str_wrap("Laju Alih Fungsi Lahan ", 9),
    size = 9
  ) +
  theme(
    plot.background = element_rect(fill = 'transparent')
  ) 

legend1



# custom legend
my_legend <- my_legend <- 
  data.frame(
    y = rep(1:3, 3),
    x = rep(1:3, each = 3),
    group = 9:1
  )  %>% 
  ggplot(aes(x = x, y = y, fill = factor(group))) +
  geom_tile() +
  # geom_text(aes(label = group)) +
  scale_fill_manual(
    values = as.vector(a$palette(3))
  ) +
  scale_x_continuous(
    breaks = 1:3,
    labels = levels(data$bi_x)
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = levels(data$bi_y)
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  ) +
  labs(
    x = str_wrap('Kepadatan Penduduk', 10),
    y = str_wrap('Laju Alih Fungsi Lahan', 10)
  ) +
  coord_equal()

my_legend




# gabung plot + legend
ggdraw() +
  draw_plot(map1, 0, 0, 1, 1) +
  draw_plot(my_legend, x = 0.67, y = 0.07, width =  0.15, 0.15, scale = 2)



# Save --------------------------------------------------------------------
ggsave(
  filename = "E:/Visualisasi/riset/bi_choropleth1.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)



