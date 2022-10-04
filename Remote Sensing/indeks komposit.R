rs_to_df <- function(rs){
  rs_df <- as(rs, "SpatialPixelsDataFrame")
  rs_df <- as.data.frame(rs_df)
  colnames(rs_df) <- c("value", "x", "y")
  rs_df
}

# ndwi = (NIR - SWIR)/(NIR + SWIR)
# ndre = (NIR - Red edge)/(NIR + Red Edge)
# gndvi = (NIR - GREEN)/(NIR + GREEN)
# ndvi = (NIR - RED)/(NIR + RED)
# ndbi = (SWIR - NIR)/(SWIR + NIR)

# SWIR = 11
# Reg edge = 8a
# NIR = 8
# RED = 4 
# GREEN = 3 
# BLUE = 2 

VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  (bk - bi) / (bk + bi)
}
SAVI <- function(img, nir, red, L = 0.5){
  nir <- img[[nir]]
  red <- img[[red]]
  (nir - red)*(1 + L) / (nir + red + L)
}
MSAVI2 <- function(img, nir, red){
  NIR <- img[[nir]]
  RED <- img[[red]]
  (2 * NIR + 1 - sqrt((2 * NIR + 1)^2 - 8 * (NIR - RED))) / 2
}
EVI <- function(img, nir, red, blue){
  NIR <- img[[nir]]
  R <- img[[red]]
  B <- img[[blue]]
  2.5 * ((NIR - R)/(1 + NIR + 6*R - 7.5*B))
}

rs@layers
bands

ndvi <- VI(rs, 8, 4)
ndwi <- VI(rs, 8, 11)
ndre <- VI(rs, 8, 9)
gndvi <- VI(rs, 8, 3)
savi <- SAVI(rs, 8, 3)
msavi2 <- MSAVI2(rs, 8, 3)
ndbi <- VI(rs, 11, 8)
evi <- EVI(rs, 8, 4, 2)


compidx <- stack(ndvi, ndwi, ndre, ndbi)
rs_idx <- stack(ndvi, savi, msavi2, ndwi, evi, ndre, gndvi, ndbi)
getValues(compidx)

names(rs_idx) <- c('ndvi', 'savi', 'msavi2', 'ndwi', 'evi', 'ndre', 'gndvi', 'ndbi')
res <- raster::predict(final_fit, object = rs_idx)
res <- raster::predict(final_fit, new_data = df_pred)

# handle NA
df_pred <- as.data.frame(getValues(rs_idx))
df_pred
colSums(is.na(df_pred))
df_pred[is.na(df_pred)] <- 0


res
rs_pred <- evi
values(rs_pred) <- res$.pred


res_df <- rs_to_df(rs_pred)

ggplot(res_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  coord_quickmap() +
  labs(
    title = 'Produktivitas Padi',
    fill = "Kw / Ha",
    x = NULL, y = NULL
  ) +
  theme_minimal() +   					    
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(size = 13),		       	   
    # axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text = element_blank()
  )  



