library(tidyverse)
library(rgeoda)
library(spdep)
library(ggrepel)
library(sf)
library(patchwork)


# Data --------------------------------------------------------------------
bb_shape <- st_read("E:/CitraRiset2/bb_LajuAlihFungsi.geojson")
pwk_shape <- st_read("E:/CitraRiset2/pwk_LajuAlihFungsi.geojson")
gab <- rbind(bb_shape, pwk_shape)

# Bandung Barat LISA -----------------------------------------------------------
# create weights object
queen_w_bb <- queen_weights(bb_shape)

# calculate LISA as per GEODA
lisa_bb <- local_moran(queen_w_bb, bb_shape["laju"])

# process results
bb_shape$cluster <- as.factor(lisa_bb$GetClusterIndicators())
levels(bb_shape$cluster) <- lisa_bb$GetLabels()

# A visual overview
bb_lisa <- ggplot(data = bb_shape) +
  geom_sf(aes(fill = cluster), color = "white") +
  geom_sf_text(
    data = ~ .x %>% 
      dplyr::filter(cluster %in% c('High-High', 'Low-Low')),
    aes(
      label = str_wrap(str_to_title(nmkec), 5)
    ),
    color = 'black',
    size = 3.5
  ) +
  scale_fill_manual(
    values = my_col,
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    subtitle = "Local Indicators for Spatial Association (LISA)",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 13, face = 1.5)
  )


bb_lisa


# Purwakarta LISA --------------------------------------------------------------
# create weights object
queen_w_pwk <- queen_weights(pwk_shape)

# calculate LISA as per GEODA
lisa_pwk <- local_moran(queen_w_pwk, pwk_shape["laju"])

# process results
pwk_shape$cluster <- as.factor(lisa_pwk$GetClusterIndicators())
levels(pwk_shape$cluster) <- lisa_pwk$GetLabels()


# pLot
pwk_lisa <-
  ggplot(data = pwk_shape) +
  geom_sf(aes(fill = cluster), color = "white") +
  geom_sf_text(
    data = ~ .x %>% 
      dplyr::filter(cluster %in% c('High-High', 'Low-Low')),
    aes(
      label = str_wrap(str_to_title(nmkec), 5)
    ),
    color = 'black',
    size = 3.5
  ) +
  scale_fill_manual(
    values = my_col,
    labels = function(x) str_wrap(x, 10)
  ) +
  scale_x_continuous(
    breaks = seq(from = 107.2, to = 107.65, by = 0.1)
  ) +
  labs(
    subtitle = "Local Indicators for Spatial Association (LISA)",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 13, face = 1.5)
  )

pwk_lisa






# Purwakarta MORAN --------------------------------------------------------------
w_pwk <- poly2nb(pwk_shape)
ww_pwk <- nb2listw(w_pwk)
pwk_moran <- moran(pwk_shape$laju, ww_pwk, n = length(ww_pwk$neighbours), S0 = Szero(ww_pwk))

# scaled laju
pwk$slaju <- scale(pwk_shape$laju)
pwk$lag_slaju <- lag.listw(ww_pwk, pwk_shape$slaju)


plot_pwk_moran <-  data.frame(
  x = pwk_shape$slaju,
  y = pwk_shape$lag_slaju,
  nmkec = pwk$nmkec
) %>%
  mutate(
    cluster = case_when(
      x < 0 & y < 0 ~ "Low-Low",
      x < 0 & y > 0 ~ "Low-High",
      x > 0 & y < 0 ~ "High-Low",
      x > 0 & y > 0 ~ "High-High",
    ),
    cluster = factor(cluster, levels = cluster_lvl)
  ) %>%
  ggplot(
    aes(x = x, y = y)
  ) +
  geom_point(aes(col = cluster)) +
  geom_vline(xintercept = 0, color = "gray60", lty = 5) +
  geom_hline(yintercept = 0, color = "gray60", lty = 5) +
  geom_smooth(method = "lm", se = F, color = "maroon") +
  geom_text_repel(
    aes(color = cluster, label = str_to_title(nmkec)),
    min.segment.length = 0,
    seed = 1,
    box.padding = 0.55
  ) +
  # geom_text(
  #   data = data.frame(
  #     x = c(0.5, 1, -1, -1, 1),
  #     y = c(1.25, -1, 1.25, -1, -0.25),
  #     text = c(
  #       cluster_lvl,
  #       paste0("Moran's I: ", round(pwk_moran$I, 3))
  #     )
  #   ),
  #   aes(x = x, y = y, label = text),
  #   fontface = 1.5,
  #   color = c(rep("royalblue3", 4), "black")
  # ) +
  scale_color_manual(
    values = c("red", "skyblue", "navy")
  ) +
  labs(
    y = "Spatial Lag",
    x = "Laju Alih Fungsi Lahan",
    subtitle = "Moran's Scatterplot"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 13, face = 1.5)
  )

plot_pwk_moran



plot_pwk_moran + pwk_lisa
ggsave(
  filename = "E:/Visualisasi/riset/pwk_moran_lisa_label.png",
  width = 15,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 0.8,
  bg = "white"
)




# Bandung Barat MORAN ----------------------------------------------------
w_bb <- poly2nb(bb_shape)
ww_bb <- nb2listw(w_bb)
bb_moran <- moran(bb_shape$laju, ww_bb, n = length(ww_bb$neighbours), S0 = Szero(ww_bb))

bb_shape$slaju <- scale(bb_shape$laju)
bb_shape$lag_slaju <- lag.listw(ww_bb, bb_shape$slaju)


plot_bb_moran <- data.frame(
  x = bb_shape$slaju,
  y = bb_shape$lag_slaju,
  nmkec = bb_shape$nmkec
) %>%
  mutate(
    cluster = case_when(
      x < 0 & y < 0 ~ "Low-Low",
      x < 0 & y > 0 ~ "Low-High",
      x > 0 & y < 0 ~ "High-Low",
      x > 0 & y > 0 ~ "High-High",
    ),
    cluster = as.factor(cluster)
  ) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(col = cluster)) +
  geom_vline(xintercept = 0, color = "gray60", lty = 5) +
  geom_hline(yintercept = 0, color = "gray60", lty = 5) +
  geom_smooth(method = "lm", se = F, color = "maroon") +
  geom_text_repel(
    aes(color = cluster, label = str_to_title(nmkec)),
    min.segment.length = 0,
    seed = 1,
    box.padding = 0.5
  ) +
  # geom_text(
  #   data = data.frame(
  #     x = c(0.3, 1, -1.2, -1.2, 0.5),
  #     y = c(1.25, -1, 1.25, -1, -0.25),
  #     text = c(
  #       "High-High", "High-Low", "Low-High", "Low-Low",
  #       paste0("Moran's I: ", round(bb_moran$I, 3))
  #     )
  #   ),
  #   aes(x = x, y = y, label = text),
  #   color = c(rep("royalblue3", 4), "black"),
  #   fontface = 1.5
  # ) +
  scale_color_manual(
    values = c("red", "orange", "skyblue", "navy")
  ) +
  labs(
    y = "Spatial Lag",
    x = "Laju Alih Fungsi Lahan",
    subtitle = "Moran's Scatterplot"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 13, face = 1.5)
  )





plot_bb_moran + bb_lisa

ggsave(
  filename = "E:/Visualisasi/riset/bb_moran_lisa_label.png",
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 0.8,
  bg = "white"
)




# Gabungan LISA ----------------------------------------------------------------
gab <- rbind(bb_shape, pwk_shape)

# create weights object
queen_w_gab <- queen_weights(gab)

# calculate LISA as per GEODA
lisa_gab <- local_moran(queen_w_gab, gab["laju"])

# process results
gab$cluster <- as.factor(lisa_gab$GetClusterIndicators())
levels(gab$cluster) <- lisa_gab$GetLabels()

# A visual overview
gab_lisa <- ggplot(data = gab) +
  geom_sf(aes(fill = cluster), color = "white") +
  geom_sf_text(
    data = ~ .x %>% 
      dplyr::filter(cluster %in% c('High-High', 'Low-Low')),
    aes(
      label = str_wrap(str_to_title(nmkec), 5)
    ),
    color = 'black',
    size = 3.5
  ) +
  scale_fill_manual(
    values = my_col,
    labels = function(x) str_wrap(x, 10)
  ) +
  labs(
    subtitle = "Local Indicators for Spatial Association (LISA)",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 13, face = 1.5)
  )

gab_lisa



# Gabungan Moran ----------------------------------------------------------
w_gab <- poly2nb(gab)
ww_gab <- nb2listw(w_gab)
gab_moran <- moran(gab$laju, ww_gab, n = length(ww_gab$neighbours), S0 = Szero(ww_gab))

gab$slaju <- scale(gab$laju)
# create a lagged variable
gab$lag_slaju <- lag.listw(ww_gab, gab$slaju)

gab_moran <- moran(gab$laju, ww_gab, n = length(ww_gab$neighbours), S0 = Szero(ww_gab))


data.frame(
  x = gab$slaju,
  y = gab$lag_slaju,
  nmkec = gab$nmkec,
  nmkab = gab$nmkab
)

# High-High High-Low Low-High Low-Low
plot_gab_moran <- data.frame(
  x = gab$slaju,
  y = gab$lag_slaju,
  nmkec = gab$nmkec
) %>%
  mutate(
    cluster = case_when(
      x < 0 & y < 0 ~ "Low-Low",
      x < 0 & y > 0 ~ "Low-High",
      x > 0 & y < 0 ~ "High-Low",
      x > 0 & y > 0 ~ "High-High",
    ),
    cluster = as.factor(cluster)
  ) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(col = cluster)) +
  geom_vline(xintercept = 0, color = "gray60", lty = 5) +
  geom_hline(yintercept = 0, color = "gray60", lty = 5) +
  geom_smooth(method = "lm", se = F, color = "maroon") +
  geom_text_repel(
    aes(color = cluster, label = str_to_title(nmkec)),
    min.segment.length = 0,
    seed = 1,
    box.padding = 0.5
  ) +
  geom_text(
    data = data.frame(
      x = c(1.5),
      y = c(-1),
      text = c(
        paste0("Moran's I: ", round(gab_moran$I, 3))
      )
    ),
    aes(x = x, y = y, label = text),
    color = c("black"),
    fontface = 1.5
  ) +
scale_color_manual(
  values = c("red", "orange", "skyblue", "navy")
) +
  labs(
    y = "Spatial Lag",
    x = "Laju Alih Fungsi Lahan",
    subtitle = "Moran's Scatterplot"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 13, face = 1.5)
  )





plot_gab_moran + gab_lisa

ggsave(
  filename = "E:/Visualisasi/riset/gabungan_moran_lisa_label.png",
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 500,
  scale = 0.8,
  bg = "white"
)


# Cara Lain LISA ----------------------------------------------------------
w <- poly2nb(gab)
ww <- nb2listw(w)

resI <- localmoran(gab$Y1, ww)
head(resI)
attr(resI, 'quadr')
