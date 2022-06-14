library(tidyverse)
library(classInt)
library(openxlsx)
library(rgeoda)
library(spdep)

library(ggrepel)
library(sf)


# color -------------------------------------------------------------------
my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')

# revisi
# my_col <- c('#b3242a', '#fba683', '#f5f5f2', "#7ccdc1", "#2f9790")
my_col <- c('#8b510a', '#e1c27b', '#F6E8C1', "#7ccdc1", "#2f9790")
my_col_lisa <- c('gray70', 'tomato', 'skyblue3', 'orange', 'steelblue', 'black', 'black')


# Data --------------------------------------------------------------------
bb_shape <- st_read("E:/CitraRiset2/bb_LajuLuas.geojson")
pwk_shape <- st_read("E:/CitraRiset2/pwk_LajuLuas.geojson")
gab <- rbind(bb_shape, pwk_shape)

gab <- st_read('E:/CitraRiset2/gab.geojson')
batas_kab <- st_read('E:/CitraRiset2/batas_kabBBPWK.geojson')


# Data Excel --------------------------------------------------------------
dat23 <- 
  read.xlsx("D:/Downloads/Bentuk Y Lainnya_Laju Alih Fungsi Lahan.xlsx") %>% 
  rename(Y2 = 'Laju.(Y2)', Y3 = 'Laju.(Y3)')
dat1 <- 
  read.xlsx("D:/Downloads/Bentuk Y pertama_Laju Alih Fungsi Lahan.xlsx") %>% 
  rename(Y1 = Laju)



# Natural Breaks ----------------------------------------------------------
gab <- gab %>% 
  mutate(nmkec = str_to_upper(nmkec)) %>% 
  left_join(dat1[c('Kecamatan', 'Y1')], by = c('nmkec' = 'Kecamatan')) %>% 
  left_join(dat23[c('Kecamatan', 'Y2', 'Y3')], by = c('nmkec' = 'Kecamatan')) %>% 
  dplyr::select(-c(idkab, idkec, kdkab, kdkec, laju))

# brY1 <- classIntervals(gab$Y1, n = 5, style = 'jenks')$br
# brY2 <- classIntervals(gab$Y2, n = 5, style = 'jenks')$br
# brY3 <- classIntervals(gab$Y3, n = 5, style = 'jenks')$br
# 
# gab <- gab %>% 
#   mutate(
#     Y1_nb = cut(Y1, breaks = brY1, include.lowest = TRUE),
#     Y2_nb = cut(Y2, breaks = brY2, include.lowest = TRUE),
#     Y3_nb = cut(Y3, breaks = brY3, include.lowest = TRUE)
#   )

get_labelBr <- function(br){
  c(
    str_glue('< {br[1]}'),
    str_glue('[{br[1]}, {br[2]})'),
    str_glue('[{br[2]}, {br[3]})'),
    str_glue('[{br[3]}, {br[4]})'),
    str_glue('>= {br[4]}')
  )
}

brY1 <- c(35.505, 44.449, 58.819, 66.419)
brY2 <- c(-7.816, -1.625, 3.126, 6.782)
brY3 <- c(-0.102, -0.016, 0.031, 0.062)
brTahunan <- c(4.438, 5.556, 7.352, 8.302)

label_brY1 <- get_labelBr(brY1)
label_brY2 <- get_labelBr(brY2)
label_brY3 <- get_labelBr(brY3)
label_brThn <- get_labelBr(brTahunan)


gab <- gab %>% 
  mutate(
    Y2 = round(Y2, 3),
    Y3 = round(Y3, 3),
    Y1_tahunan = round(y1_tahunan, 3),
    
    
    Y1_nb = case_when(
      Y1 < brY1[1] ~ 1,
      Y1 >= brY1[1] & Y1 < brY1[2] ~ 2,
      Y1 >= brY1[2] & Y1 < brY1[3] ~ 3,
      Y1 >= brY1[3] & Y1 < brY1[4] ~ 4,
      Y1 >= brY1[4] ~ 5
    ),
    Y2_nb = case_when(
      Y2 < brY2[1] ~ 1,
      Y2 >= brY2[1] & Y2 < brY2[2] ~ 2,
      Y2 >= brY2[2] & Y2 < brY2[3] ~ 3,
      Y2 >= brY2[3] & Y2 < brY2[4] ~ 4,
      Y2 >= brY2[4] ~ 5
    ),
    Y3_nb = case_when(
      Y3 < brY3[1] ~ 1,
      Y3 >= brY3[1] & Y3 < brY3[2] ~ 2,
      Y3 >= brY3[2] & Y3 < brY3[3] ~ 3,
      Y3 >= brY3[3] & Y3 < brY3[4] ~ 4,
      Y3 >= brY3[4] ~ 5
    ),
    Y1_thn_nb = case_when(
      y1_tahunan < brTahunan[1] ~ 1,
      y1_tahunan >= brTahunan[1] & y1_tahunan < brTahunan[2] ~ 2,
      y1_tahunan >= brTahunan[2] & y1_tahunan < brTahunan[3] ~ 3,
      y1_tahunan >= brTahunan[3] & y1_tahunan < brTahunan[4] ~ 4,
      y1_tahunan >= brTahunan[4] ~ 5
    ),
    
    Y1_nb = factor(Y1_nb, levels = 1:5, labels = label_brY1),
    Y2_nb = factor(Y2_nb, levels = 1:5, labels = label_brY2),
    Y3_nb = factor(Y3_nb, levels = 1:5, labels = label_brY3),
    Y1_thn_nb = factor(Y1_thn_nb, levels = 1:5, labels = label_brThn)
  )

# Centroid ----------------------------------------------------------------
cent <- 
  st_coordinates(st_centroid(gab)) %>% 
  as_tibble() %>% 
  mutate(
    nmkec = str_to_title(gab$nmkec)
  )

gab$X_cen <- cent$X
gab$Y_cen <- cent$Y


# Batas Kab ---------------------------------------------------------------
# batas_kab <- rbind(st_union(pwk_shape), st_union(bb_shape))
# batas_kab
# 
# batas_kab <- data.frame(nmkab = unique(gab$nmkab))
# batas_kab$geom = st_sfc(c(st_union(bb_shape), st_union(pwk_shape)))
# batas_kab <- st_as_sf(batas_kab)
# batas_kab

# Peta Y1 ------------------------------------------------------------------



ggplot(data = gab) +
  geom_sf(
    aes(fill = Y1_nb), color = "white",
    size = 0.3
  ) +
  geom_sf(
    data = batas_kab,
    color = 'gray30',
    fill = 'transparent',
    size = 0.55,
    inherit.aes = F
  ) +
  geom_label_repel(
    data = subset(gab, X_cen < 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    xlim = c(107, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 0,
    box.padding = 0.17,
    nudge_x = 107 - subset(gab, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 3 # n control point
  ) +
  geom_label_repel(
    data = subset(gab, X_cen >= 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    xlim = c(-Inf, 107.9), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 1,
    box.padding = 0.17,
    nudge_x = 107 + subset(gab, X_cen >= 107.45)$X_cen,
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
    values = my_col
  ) +
  # scale_fill_brewer(
  #   palette = 'RdYlGn',
  # ) +
  labs(
    title = 'Laju Persentase Alih Fungsi Lahan Sawah Tahunan',
    # title = 'Peta Klasifikasi Laju Perubahan Lahan Sawah Tahunan',
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta Tahun 2013-2021',
    # fill = str_wrap('Kelas Laju Perubahan Lahan Sawah Tahunan Natural Break (%)', 25), 
    fill = str_wrap('Kelas Laju Persentase Alih Fungsi Lahan Sawah Tahunan Natural Break (%)', 25),
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(106.99, 107.9), ylim = c(-6.3, -7.2))



ggsave(
  filename = "E:/Visualisasi/riset/Y1/y1_laju_warna.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)
#



# Indeks Moran ------------------------------------------------------------
w_gab <- poly2nb(gab)
ww_gab <- nb2listw(w_gab)

gab_moran1 <- moran(gab$Y1, ww_gab, n = length(ww_gab$neighbours), S0 = Szero(ww_gab))
gab_moran2 <- moran(gab$Y2, ww_gab, n = length(ww_gab$neighbours), S0 = Szero(ww_gab))
gab_moran3 <- moran(gab$Y3, ww_gab, n = length(ww_gab$neighbours), S0 = Szero(ww_gab))
gab_moran4 <- moran(gab$y1_tahunan, ww_gab, n = length(ww_gab$neighbours), S0 = Szero(ww_gab))


dat <- data.frame(
  x1 = scale(gab$Y1),
  x2 = scale(gab$Y2),
  x3 = scale(gab$Y3),
  x4 = scale(gab$Y1_tahunan),
  nmkec = str_to_title(gab$nmkec)
) %>% 
  mutate(
    y1 = lag.listw(ww_gab, x1),
    y2 = lag.listw(ww_gab, x2),
    y3 = lag.listw(ww_gab, x3),
    y4 = lag.listw(ww_gab, x4),
    .before = nmkec
  )

get_cluster <- function(x, y){
  return(
    case_when(
      x < 0 & y < 0 ~ "Low-Low",
      x < 0 & y > 0 ~ "Low-High",
      x > 0 & y < 0 ~ "High-Low",
      x > 0 & y > 0 ~ "High-High",
    )
  )
}

cluster_lvl <- c("High-High", "High-Low", "Low-High", "Low-Low")
dat <- dat %>% 
  mutate(
    cluster1 = get_cluster(x1, y1),
    cluster2 = get_cluster(x2, y2),
    cluster3 = get_cluster(x3, y3),
    cluster4 = get_cluster(x4, y4),
    across(starts_with('cluster'), ~ factor(.x, levels = cluster_lvl))
  )

dat %>% 
  glimpse()

lm1 <- lm(y2 ~ x2, data = dat)
lm1
# 
lm1$coefficients[1] + 2 * lm1$coefficients[2]


# High-High High-Low Low-High Low-Low
dat %>% 
  # ganti ini
  ggplot(aes(x = x1, y = y1)) +
  geom_point(aes(col = cluster1), size = 2.5) +
  
  geom_vline(xintercept = 0, color = "gray60", lty = 5) +
  geom_hline(yintercept = 0, color = "gray60", lty = 5) +
  geom_smooth(method = "lm", se = F, color = "maroon") +
  
  geom_text_repel(
    data = ~ .x %>%
      filter(
        y1 >= lm1$coefficients[1] + x1 * lm1$coefficients[2]
      ),
    # ganti ini
    aes(color = cluster1, label = str_to_title(nmkec)),
    size = 3,
    min.segment.length = 0,
    seed = 0,
    max.time = 1, 
    max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
    box.padding = 0.4, # additional padding around each text label
    max.overlaps = Inf,
    direction = 'both',
    vjust = 0,
    hjust = 0,
    segment.curvature = -1e-20,
    segment.size = 0.15,
    segment.ncp = 3,
  ) +
  geom_text_repel(
    data = ~ .x %>%
      filter(
        y1 < lm1$coefficients[1] + x1 * lm1$coefficients[2]
      ),
    # ganti ini
    aes(color = cluster1, label = str_to_title(nmkec)),
    seed = 0,
    size = 3,
    min.segment.length = 0,
    max.time = 1, 
    max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
    box.padding = 0.4, # additional padding around each text label
    max.overlaps = Inf,
    direction = 'both',
    hjust = 1,
    segment.curvature = -1e-20,
    segment.size = 0.15,
    segment.ncp = 3,
  ) +
  scale_color_manual(
    values = c("red", "orange", "skyblue3", "navy")
  ) +
  labs(
    y = "Spatial Lag",
    x = "Laju Alih Fungsi Lahan",
    title = "Scatterplot Moran's I Y1",
    subtitle = paste0("Moran's I: ", round(gab_moran1$I, 3))
  ) +
  theme_bw(base_family = 'Arial') +
  theme(
    legend.position = "none",
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  xlim(c(-2, 2))

ggsave(
  filename = "E:/Visualisasi/riset/Y1/moran.png",
  width = 7,
  height = 6,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)

#


# LISA --------------------------------------------------------------------
queen_w <- queen_weights(gab)

# calculate LISA as per GEODA
lisaY1 <- local_moran(queen_w, gab["Y1"])
lisaY2 <- local_moran(queen_w, gab["Y2"])
lisaY3 <- local_moran(queen_w, gab["Y3"])

label <- lisaY1$GetLabels()

gab <- gab %>%  
  mutate(
    cluster1 = factor(lisaY1$GetClusterIndicators(), levels = 0:6, labels = label),
    cluster2 = factor(lisaY2$GetClusterIndicators(), levels = 0:6, labels = label),
    cluster3 = factor(lisaY3$GetClusterIndicators(), levels = 0:6, labels = label)
  ) 

gab

lisaY2$p_vals

# gab$cluster1 <- factor(lisaY1$GetClusterIndicators(), levels = 0:6, labels = label)
# levels(gab$cluster1) <- lisaY1$GetLabels()




# Y1
ggplot(data = gab) +
  geom_sf(
    aes(fill = cluster1), 
    color = "white",
    size = 0.15
  ) +
  geom_sf(
    data = batas_kab,
    color = 'gray30',
    fill = 'transparent',
    size = 0.5,
    inherit.aes = F
  ) +
  geom_point(
    data = ~.x %>% 
      dplyr::filter(!cluster1 %in% c('Not significant')),
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    inherit.aes = F,
    size = 1
  ) +
  geom_label_repel(
    data = ~.x %>% 
      dplyr::filter(
        !cluster1 %in% c('Not significant')
      ),
    aes(
      label = str_wrap(str_to_title(nmkec), 15),
      x = X_cen, y = Y_cen
    ), 
    force = 0.5,
    xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    seed = 1,
    direction = 'both',
    box.padding = 0.9,
    inherit.aes = F,
    max.overlaps = Inf,
    size = 3,
    max.iter = 1e4,
    max.time = 1,
    segment.curvature = -1e-20,
    segment.ncp = 5
  ) +
  scale_fill_manual(
    values = c('gray75', 'tomato', 'dodgerblue3', 'orange', 'steelblue', 'black', 'black'),
    # values = c('gray', "red", 'navy', 'orange', 'skyblue3','black', 'black'),
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    # title = "Peta Klaster Lisa Laju Perubahan Lahan Sawah Tahunan",
    title = "Peta Klaster LISA Laju Persentase Alih Fungsi Lahan Sawah Tahunan",
    subtitle = 'Kabupaten Bandung Barat dan Kabupaten Purwakarta',
    x = 'Longitude', y = 'Latitude', fill = 'cluster'
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(107.1, 107.79), ylim = c(-6.35, -7.15))


#


# LISA Y2 -----------------------------------------------------------------

ggplot(data = gab) +
  geom_sf(
    aes(fill = cluster2), 
    color = "white",
    size = 0.15
  ) +
  geom_sf(
    data = batas_kab,
    color = 'gray30',
    fill = 'transparent',
    size = 0.5,
    inherit.aes = F
  ) +
  geom_point(
    data = ~.x %>% 
      dplyr::filter(!cluster2 %in% c('Not significant')),
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    inherit.aes = F,
    size = 1
  ) +
  geom_label_repel(
    data = ~.x %>% 
      dplyr::filter(
        !cluster2 %in% c('Not significant')
      ),
    aes(
      label = str_wrap(str_to_title(nmkec), 15),
      x = X_cen, y = Y_cen
    ), 
    force = 0.5,
    xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    seed = 1,
    direction = 'both',
    box.padding = 0.9,
    inherit.aes = F,
    max.overlaps = Inf,
    size = 3,
    max.iter = 1e4,
    max.time = 1,
    segment.curvature = -1e-20,
    segment.ncp = 5
  ) +
  scale_fill_manual(
    values = c('gray75', 'tomato', 'dodgerblue3', 'orange', 'steelblue', 'black', 'black'),
    # values = c('gray', "red", 'navy', 'orange', 'skyblue3','black', 'black'),
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    title = "Klaster LISA Y2",
    subtitle = 'Kabupaten Bandung Barat dan Purwakarta',
    x = 'Longitude', y = 'Latitude', fill = 'cluster'
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(107.1, 107.79), ylim = c(-6.35, -7.15))


#

# LISA Y3 -----------------------------------------------------------------


ggplot(data = gab) +
  geom_sf(aes(fill = cluster3), color = "white") +
  geom_label_repel(
    data = ~.x %>% dplyr::filter(!cluster3 %in% c('Not significant')),
    aes(
      label = str_wrap(str_to_title(nmkec), 15),
      x = X_cen, y = Y_cen
    ), 
    vjust = 0.5,
    xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    seed = 1,
    direction = 'both',
    box.padding = 1,
    inherit.aes = F,
    max.overlaps = 20,
    size = 3
  ) +
  geom_point(
    data = ~.x %>% dplyr::filter(!cluster3 %in% c('Not significant')),
    aes(
      x = X_cen, y = Y_cen
    ),
    color = 'black',
    inherit.aes = F,
    size = 1
  ) +
  scale_fill_manual(
    values = my_col_lisa,
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    title = "Klaster LISA Y1",
    subtitle = 'Kabupaten Bandung Barat dan Purwakarta',
    x = 'Longitude', y = 'Latitude', fill = 'cluster'
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 13, face = 1.5)
  ) +
  coord_sf(xlim = c(107, 107.8), ylim = c(-6.4, -7.2))


# Save lisa ---------------------------------------------------------------


ggsave(
  filename = "E:/Visualisasi/riset/Y1/y1_lisa_rev.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)




# Estimasi Luas Lahan -----------------------------------------------------
brLS <- classIntervals(gab$estimasi_luas, n = 5, style = 'jenks')$br

gab <- gab %>%
  mutate(
    ls = cut(estimasi_luas, breaks = brLS, include.lowest = TRUE)
  )

gab %>% 
  mutate(
    ls
  )

levels(gab$ls)
my_label <- c('[435, 1280]', '(1280, 1990]', '(1990, 2870]', '(2870, 3770]', '(3770, 4910]')
my_label

ggplot(data = gab) +
  geom_sf(
    aes(fill = ls), color = "white",
    size = 0.15
  ) +
  geom_label_repel(
    data = subset(gab, X_cen < 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    xlim = c(107, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 0,
    box.padding = 0.2,
    nudge_x = 107 - subset(gab, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 5 # n control point
  ) +
  geom_label_repel(
    data = subset(gab, X_cen >= 107.45),
    aes(
      label = str_wrap(nmkec, 15),
      x = X_cen, y = Y_cen
    ), 
    xlim = c(-Inf, 107.9), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    size = 2,
    seed = 1,
    hjust = 1,
    box.padding = 0.2,
    nudge_x = 107 + subset(gab, X_cen >= 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e5,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 5 # n control point
  ) +
  geom_point(
    data = cent,
    aes(
      x = X, y = Y
    ),
    color = 'black',
    inherit.aes = F,
    size = 0.7
  ) +
  scale_fill_manual(
    values = rev(my_col),
    labels = my_label
  ) +
  labs(
    subtitle = 'Kabupaten Bandung Barat dan Purwakarta',
    fill = 'Luas (Ha)', 
    title = 'Estimasi Luas Pertanian/Sawah Tahun 2021',
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_minimal(base_family = 'Arial', base_size = 13) +
  theme(
    plot.title = element_text(face = 'bold'),
    plot.subtitle = element_text(colour = 'gray30')
  ) +
  coord_sf(xlim = c(106.95, 107.9), ylim = c(-6.3, -7.2))




ggsave(
  filename = "E:/Visualisasi/riset/estimasi_luas.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)







# Revisi Moran ------------------------------------------------------------
dat %>% 
  # ganti ini
  ggplot(aes(x = x1, y = y1)) +
  geom_point(aes(col = cluster1), size = 2.5) +
  
  geom_vline(xintercept = 0, color = "gray60", lty = 5) +
  geom_hline(yintercept = 0, color = "gray60", lty = 5) +
  geom_smooth(method = "lm", se = F, color = "maroon") +
  
  geom_text_repel(
    # ganti ini
    aes(color = cluster1, label = str_to_title(nmkec)),
    size = 3,
    min.segment.length = 0,
    seed = 0,
    max.time = 1, 
    max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
    box.padding = 0.4, # additional padding around each text label
    max.overlaps = Inf,
    direction = 'both',
    segment.curvature = -1e-20,
    segment.size = 0.15,
    segment.ncp = 3,
  ) +
  scale_color_manual(
    values = c("red", "orange", "skyblue3", "navy")
  ) +
  coord_cartesian(clip = 'off', xlim = c(-2, 2)) +
  labs(
    y = "Spatial Lag",
    x = "Laju Alih Fungsi Lahan",
    # title = "Scatterplot Moran's I Laju Perubahan Lahan Sawah Tahunan",
    title = "Scatterplot Moran's I Laju Persentase Alih Fungsi Lahan Sawah Tahunan",
    subtitle = paste0("Moran's I: ", round(gab_moran1$I, 4))
  ) +
  theme_bw(base_family = 'Arial') +
  theme(
    legend.position = "none",
    plot.title = element_text(face = 2),
    plot.subtitle = element_text(colour = 'gray30'),
    plot.margin = margin(5.5, r = 15, 5.5, 5.5, "points")
  ) 





ggsave(
  filename = "E:/Visualisasi/riset/Y1/y1_moran_rev.png",
  width = 7.5,
  height = 6,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)

