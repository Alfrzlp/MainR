library(tidyverse)
library(classInt)
library(openxlsx)
library(ggrepel)
library(rgeoda)
library(spdep)
library(sf)
library(patchwork)


# color -------------------------------------------------------------------
my_col <- c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA')
my_col_lisa <- c('gray70', 'tomato', 'skyblue3', 'orange', 'steelblue', 'black', 'black')


# Data --------------------------------------------------------------------
bb_shape <- st_read("E:/CitraRiset2/bb_LajuAlihFungsi.geojson")
pwk_shape <- st_read("E:/CitraRiset2/pwk_LajuAlihFungsi.geojson")
gab <- rbind(bb_shape, pwk_shape)


sf::write_sf(gab, "E:/CitraRiset2/shp_kbb_pwk/kbb_pwk.shp")

# Data Excel --------------------------------------------------------------
dat23 <- 
  read.xlsx("D:/Downloads/Bentuk Y Lainnya_Laju Alih Fungsi Lahan.xlsx") %>% 
  rename(Y2 = 'Laju.(Y2)', Y3 = 'Laju.(Y3)')
dat1 <- 
  read.xlsx("D:/Downloads/Bentuk Y pertama_Laju Alih Fungsi Lahan.xlsx") %>% 
  rename(Y1 = Laju)



# Natural Breaks ----------------------------------------------------------
gab <- gab %>% 
  left_join(dat1[c('Kecamatan', 'Y1')], by = c('nmkec' = 'Kecamatan')) %>% 
  left_join(dat23[c('Kecamatan', 'Y2', 'Y3')], by = c('nmkec' = 'Kecamatan')) %>% 
  dplyr::select(-c(idkab, idkec, kdkab, kdkec, laju))

brY1 <- classIntervals(gab$Y1, n = 5, style = 'jenks')$br
brY2 <- classIntervals(gab$Y2, n = 5, style = 'jenks')$br
brY3 <- classIntervals(gab$Y3, n = 5, style = 'jenks')$br

gab <- gab %>% 
  mutate(
    Y1_nb = cut(Y1, breaks = brY1, include.lowest = TRUE),
    Y2_nb = cut(Y2, breaks = brY2, include.lowest = TRUE),
    Y3_nb = cut(Y3, breaks = brY3, include.lowest = TRUE)
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

# Peta Y1 ------------------------------------------------------------------


ggplot(data = gab) +
  geom_sf(aes(fill = Y1_nb), color = "white") +
  geom_text_repel(
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
    box.padding = 0.45,
    nudge_x = 107 - subset(gab, X_cen < 107.45)$X_cen,
    inherit.aes = F,
    max.overlaps = Inf,
    max.iter = 1e4,
    max.time = 1,
    segment.size = 0.15,
    segment.curvature = -1e-20, # biar sharp angle
    segment.ncp = 5 # n control point
  ) +
  geom_text_repel(
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
    box.padding = 0.45,
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
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    subtitle = NULL,
    fill = NULL, 
    title = NULL,
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 13, face = 1.5)
  ) +
  coord_sf(xlim = c(106.95, 107.9), ylim = c(-6.3, -7.2))




ggsave(
  filename = "E:/Visualisasi/riset/Y1/y.png",
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


dat <- data.frame(
  x1 = scale(gab$Y1),
  x2 = scale(gab$Y2),
  x3 = scale(gab$Y3),
  nmkec = str_to_title(gab$nmkec)
) %>% 
  mutate(
    y1 = lag.listw(ww_gab, x1),
    y2 = lag.listw(ww_gab, x2),
    y3 = lag.listw(ww_gab, x3),
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
    across(starts_with('cluster'), ~ factor(.x, levels = cluster_lvl))
  )

dat %>% 
  glimpse()


# High-High High-Low Low-High Low-Low
dat %>% 
  ggplot(aes(x = x3, y = y3)) +
  geom_point(aes(col = cluster3), size = 2.5) +
  geom_vline(xintercept = 0, color = "gray60", lty = 5) +
  geom_hline(yintercept = 0, color = "gray60", lty = 5) +
  geom_smooth(method = "lm", se = F, color = "maroon") +
  geom_text_repel(
    aes(color = cluster3, label = str_to_title(nmkec)),
    min.segment.length = 0,
    seed = 1,
    box.padding = 1,
    max.overlaps = 20,
    direction = 'y'
  ) +
  scale_color_manual(
    values = c("red", "orange", "skyblue3", "navy")
  ) +
  labs(
    y = "Spatial Lag",
    x = "Laju Alih Fungsi Lahan",
    title = "Moran's Scatterplot",
    subtitle = paste0("Moran's I: ", round(gab_moran3$I, 3))
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = "E:/Visualisasi/riset/Y3/moran.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1.2,
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
    cluster1 = as.factor(lisaY1$GetClusterIndicators()),
    cluster2 = as.factor(lisaY2$GetClusterIndicators()),
    cluster3 = as.factor(lisaY3$GetClusterIndicators())
  ) %>% 
  mutate_at(vars(starts_with('cluster')), ~ `levels<-`(., label))

gab

  



# Y1
ggplot(data = gab) +
  geom_sf(aes(fill = cluster1), color = "white") +
  geom_label_repel(
    data = ~.x %>% 
      dplyr::filter(!cluster1 %in% c('Not significant')),
    aes(
      label = str_wrap(str_to_title(nmkec), 15),
      x = X_cen, y = Y_cen
    ), 
    force = 0.5,
    xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 0,
    seed = 1,
    direction = 'both',
    box.padding = 1,
    inherit.aes = F,
    max.overlaps = Inf,
    size = 3,
    max.iter = 1e4,
    max.time = 1,
    segment.curvature = -1e-20,
    segment.ncp = 5
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
  scale_fill_manual(
    values = my_col_lisa,
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    subtitle = "Local Indicators for Spatial Association (LISA)",
    title = NULL, x = 'Longitude', y = 'Latitude', fill = 'cluster'
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 13, face = 1.5)
  ) +
  coord_sf(xlim = c(106.95, 107.9), ylim = c(-6.3, -7.2))




# Y2
ggplot(data = gab) +
  geom_sf(aes(fill = cluster2), color = "white") +
  geom_label_repel(
    data = ~.x %>% dplyr::filter(!cluster2 %in% c('Not significant')),
    aes(
      label = str_wrap(str_to_title(nmkec), 15),
      x = X_cen, y = Y_cen
    ), 
    force = 0.5,
    xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
    min.segment.length = 1,
    seed = 1,
    direction = 'both',
    box.padding = 1,
    inherit.aes = F,
    max.overlaps = 20,
    size = 3
  ) +
  geom_point(
    data = ~.x %>% dplyr::filter(!cluster2 %in% c('Not significant')),
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
    subtitle = "Local Indicators for Spatial Association (LISA)",
    title = NULL, x = 'Longitude', y = 'Latitude', fill = 'cluster'
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 13, face = 1.5)
  ) +
  coord_sf(xlim = c(107, 107.8), ylim = c(-6.4, -7.2))



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
    subtitle = "Local Indicators for Spatial Association (LISA)",
    title = NULL, x = 'Longitude', y = 'Latitude', fill = 'cluster'
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 13, face = 1.5)
  ) +
  coord_sf(xlim = c(107, 107.8), ylim = c(-6.4, -7.2))


ggsave(
  filename = "E:/Visualisasi/riset/Y3/lisa.png",
  width = 7,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 1,
  bg = "white"
)
