library(sf)
library(ggrepel)
library(tidyverse)
library(classInt)


# -------------------------------------------------------------------------
windowsFonts(
  poppins = windowsFont('poppins'),
  tnr = windowsFont('Times New Roman')
)
lab_tokomma <- function(y_lab, arah = 'S'){
  y_lab <- str_replace_all(y_lab, '\\.', ',')
  y_lab <- str_glue('{y_lab}°{arah}')
  return(y_lab)
}


my_col <- c('#20AC4B', '#A4D29F', '#FFFFB5', "#f7a05e", "#E40017")

# data --------------------------------------------------------------------
gab <- st_read('D:/__Datasets/Riset2/gab_rev2.geojson') %>% 
  mutate(nmkec = str_to_title(nmkec)) %>% 
  select(nmkab, nmkec, Y1, X_cen, Y_cen)
head(gab)

batas_kab <- st_read('D:/__Datasets/Riset2/batas_kabBBPWK.geojson')

dat <- readxl::read_xlsx(
  'D:/Downloads/Persentase Perubahan Tutupan Lahan Sawah Tahunan.xlsx'
) %>% 
  rename(
    p = `Persentase Perubahan Tutupan Lahan Sawah Tahunan`,
    nmkec = R103N
  ) %>% 
  mutate(
    nmkec = str_to_title(nmkec)
  )
  

glimpse(dat)
setdiff(dat$nmkec, gab$nmkec)
setdiff(gab$nmkec, dat$nmkec)


gab <- gab %>% 
  left_join(
    dat, by = 'nmkec'
  )

bb <- gab %>% dplyr::filter(nmkab == 'Bandung Barat')
pwk <- gab %>% dplyr::filter(nmkab == 'Purwakarta')



# Natural Breakas ---------------------------------------------------------
br_bb <- classIntervals(bb$p, n = 5, style = 'jenks')$br
br_pwk <- classIntervals(pwk$p, n = 5, style = 'jenks')$br


br_bb <- c(-6, -2, 2, 6)
br_pwk <- c(-6, -2, 2, 6)


get_labelBr <- function(br){
  br <- scales::dollar(br, decimal.mark = ',', big.mark = '.', prefix = '')
  c(
    str_glue('< {br[1]}'),
    str_glue('[{br[1]}; {br[2]})'),
    str_glue('[{br[2]}; {br[3]})'),
    str_glue('[{br[3]}; {br[4]})'),
    str_glue('>= {br[4]}')
  )
}

get_labelBr(br_bb)

bb <- bb %>% 
  mutate(
    p = round(p, 3),
    p_nb = case_when(
      p < br_bb[1] ~ 1,
      p >= br_bb[1] & p < br_bb[2] ~ 2,
      p >= br_bb[2] & p < br_bb[3] ~ 3,
      p >= br_bb[3] & p < br_bb[4] ~ 4,
      p >= br_bb[4] ~ 5
    ),
    p_nb = factor(p_nb, levels = 1:5, labels = get_labelBr(br_bb))
  )

pwk <- pwk %>% 
  mutate(
    p = round(p, 3),
    p_nb = case_when(
      p < br_pwk[1] ~ 1,
      p >= br_pwk[1] & p < br_pwk[2] ~ 2,
      p >= br_pwk[2] & p < br_pwk[3] ~ 3,
      p >= br_pwk[3] & p < br_pwk[4] ~ 4,
      p >= br_pwk[4] ~ 5
    ),
    p_nb = factor(p_nb, levels = 1:5, labels = get_labelBr(br_pwk))
  )


# PWK ----------------------------------------------------------------------
my_family <- 'tnr'

ggplot(data = pwk) +
  geom_sf(
    aes(fill = p_nb), color = "white",
    size = 0.3
  ) +
  geom_sf(
    data = dplyr::filter(batas_kab, nmkab == 'Purwakarta'),
    color = 'gray30',
    fill = 'transparent',
    size = 0.55,
    inherit.aes = F
  ) +
  geom_label_repel(
    data = ~ subset(.x, X_cen < 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    family = my_family,
    xlim = c(107, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 0,
    box.padding = 0.17,
    nudge_x = 107 - subset(pwk, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_label_repel(
    data = ~ subset(.x, X_cen >= 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    family = my_family,
    xlim = c(-Inf, 107.9), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 1, 
    box.padding = 0.17,
    nudge_x = 107 + subset(pwk, X_cen >= 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e5,
    max.time = 1,
    segment.angle = 90,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_point(
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    inherit.aes = F,
    size = 0.7
  ) +
  scale_fill_manual(
    values = rev(my_col),
    drop = F
  ) +
  labs(
    title = 'Persentase Perubahan Tutupan Lahan Sawah Tahunan',
    subtitle = 'Kabupaten Purwakarta Tahun 2013-2021',
    fill = str_wrap('Kelas Persentase Perubahan Tutupan Lahan Sawah Tahunan (%)', 25),
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_bw(base_family = my_family) +
  theme(
    plot.title = element_text(face = 2, vjust = 0),
    plot.subtitle = element_text(colour = 'gray30', vjust = 0),
    axis.text.y = element_text(margin = margin(l = 0))
  ) +
  coord_sf(xlim = c(106.99, 107.9), ylim = c(-6.33, -6.83)) +
  scale_x_continuous(
    labels = lab_tokomma(seq(107, 107.8, by = 0.2), 'E')
  ) +
  scale_y_continuous(
    labels = lab_tokomma(seq(6.8, 6.4, by = -0.1))
  )



ggsave(
  filename = str_glue("E:/Visualisasi/riset/perubahan/pwk_{my_family}.png"),
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)




# BB ----------------------------------------------------------------------
ggplot(data = bb) +
  geom_sf(
    aes(fill = p_nb), color = "white",
    size = 0.3
  ) +
  geom_sf(
    data =  dplyr::filter(batas_kab, nmkab == 'Bandung Barat'),
    color = 'gray30',
    fill = 'transparent',
    size = 0.55,
    inherit.aes = F
  ) +
  geom_label_repel(
    data = ~ subset(.x, X_cen < 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    family = my_family,
    xlim = c(107, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 0,
    box.padding = 0.17,
    nudge_x = 107 - subset(bb, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_label_repel(
    data = ~ subset(.x, X_cen >= 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    family = my_family,
    xlim = c(-Inf, 107.9), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 1,
    box.padding = 0.17,
    nudge_x = 107 + subset(bb, X_cen >= 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e5,
    max.time = 1,
    segment.angle = 90,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_point(
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    inherit.aes = F,
    size = 0.7
  ) +
  scale_fill_manual(
    values = my_col2
  ) +
  labs(
    title = 'Persentase Perubahan Tutupan Lahan Sawah Tahunan',
    subtitle = 'Kabupaten Bandung Barat Tahun 2013-2021',
    fill = str_wrap('Kelas Persentase Perubahan Tutupan Lahan Sawah Tahunan (%)', 25),
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_bw(base_family = my_family) +
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(106.99, 107.9), ylim = c(-6.65, -7.15)) +
  scale_x_continuous(
    labels = lab_tokomma(seq(107, 107.8, by = 0.2), 'E')
  ) +
  scale_y_continuous(
    labels = lab_tokomma(seq(7.1, 6.7, by = -0.1))
  )



ggsave(
  filename = str_glue("E:/Visualisasi/riset/perubahan/bb_{my_family}.png"),
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)

#


# To dashboard ------------------------------------------------------------
bb %>% 
  select(nmkab, nmkec, p_nb, p) %>% 
  write_sf('D:/xampp/htdocs/web/peta-perubahan/bandung_barat/bb.geojson')

pwk %>% 
  select(nmkab, nmkec, p_nb, p) %>% 
  write_sf('D:/xampp/htdocs/web/peta-perubahan/purwakarta/pwk.geojson')

dput(levels(pwk$p_nb))
dput(rev(my_col))
