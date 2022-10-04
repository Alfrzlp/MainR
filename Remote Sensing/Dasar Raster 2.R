library(tidyverse)
library(raster)
library(viridis)
library(sf)

rs <- stack('D:/__Skripsi/kotak_2020_4v2.tif')
rs

b8 <- raster(rs, layer = 8)
# ndwi = (b8 - b11)/(b8 + b11),
# ndre = (b8 - b8a)/(b8 + b8a),
# gndvi = (b8 - b3)/(b8 + b3),
# ndvi = (b8 - b4)/(b8 + b4)

nlayers(rs)
minValue(rs)
maxValue(rs)
rs@layers
# nrow(rs) * ncol(rs) == ncell(rs)

# Note that the plot function only plots 100,000 pixels 
# but image strectches the view.
plotRGB(rs, r = 4, g = 3, b = 2, stretch = "lin")
plotRGB(rs, r = 4, g = 3, b = 2, stretch = "hist")

image(
  b8, col = viridis_pal(option="D")(10),
  main="Sentinel 2"
)


# Cara 1 ------------------------------------------------------------------
png('E:/Visualisasi/rasterb8.png',
    width = 8, height = 6,
    units = "in", res = 300) 
image(
  b8, col = viridis_pal(option="D")(10),
  main="Sentinel 2"
)
dev.off() 


# Cara 2: ggplot ----------------------------------------------------------
b8_df <- as(b8, "SpatialPixelsDataFrame")
b8_df <- as.data.frame(b8_df)
colnames(b8_df) <- c("value", "x", "y")

ggplot(b8_df) +
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
    title = 'Sentinel B8',
    x = NULL, y = NULL
  ) +
  theme_minimal() +   					    
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(size = 13),		       	   
    # axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text = element_blank()
  )  

# panel kotakan plot
# plot itu semuanya termasuk legend dll

ggsave(
  "E:/Visualisasi/rasterb8_v2.png",
  scale = 1.7,
  dpi = 500,
  bg = 'white'
) 

# viz beberapa ----------------------------------------------------------------
compidx <- stack(ndvi, ndwi, ndre, ndbi)
ci_df <- as(compidx, "SpatialPixelsDataFrame")
ci_df <- as.data.frame(ci_df)
colnames(ci_df) <- c('ndvi', 'ndwi', 'ndre', 'ndbi', "x", "y")

ci_df %>% 
  pivot_longer(-c(x, y), names_to = 'ci') %>% 
  ggplot() +
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
    title = 'Sentinel-2A MSI',
    x = NULL, y = NULL
  ) +
  theme_minimal() +   					    
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(size = 13),		       	   
    # axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text = element_blank()
  ) +
  facet_wrap(
    vars(ci),
    labeller = labeller(.rows = str_to_upper)
  )



# raster manipulation -----------------------------------------------------
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}
# For Sentinel 2, the relevant bands to use are:
# NIR = 8, red = 4

ndvi <- VI(rs, 8, 4)
plot(ndvi)


# We are reclassifying our object and making all values between
# negative infinity and 0.4 be NAs
veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
plot(veg, main = 'Veg cover')
image(veg, col = viridis_pal(option="D")(10))


# Write raster ------------------------------------------------------------
# Saving the data as integers rather than floats requires
# less memory and processing for the computer to handle
writeRaster(
  x = ndvi,
  filename = "D:/__Skripsi/citra/ndvi.tif", 	
  format = "GTiff", 					
  # datatype = 'INT2S'
) 


# convert to vector
getValues(ndvi) 
getValues(veg)      
