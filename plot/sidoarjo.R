library(rgdal)
library(rgeos)
library(maptools)

indo <- readOGR(
  dsn = "D:/Datasets/gadm36_IDN_shp", layer = "gadm36_IDN_3",
  stringsAsFactors = F
)

indonesia <- fortify(indo, region = "NAME_3")
dataindo <- indo@data
colnames(dataindo)[10] <- "id"

library(dplyr)
indonesia <- left_join(indonesia, dataindo, by = "id")

# plot indonesia perkecamatan
library(ggplot2)
ggplot(indonesia, aes(x = long, y = lat)) +
  geom_map(map = indonesia, aes(map_id = id), color = "white", size = 0.5)

# ambil sidoarjo
sidoarjo <- indonesia %>% filter(NAME_2 == "Sidoarjo")
sidoarjo

# write.csv(sidoarjo, file="peta sidoarjo.csv")

# ========================================================================
# setelah di simpan di csv
sidoarjo <- read.csv("peta sidoarjo.csv")
sidoarjo

library(ggplot2)
kab <- sidoarjo %>%
  group_by(id) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(sidoarjo, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = id, group = group), color = "transparent") +
  xlim(112.45, 112.9) +
  ylim(-7.6, -7.3) +
  geom_text(aes(x = long, y = lat, label = id), col = "white", data = kab, size = 3) +
  scale_fill_viridis_d(
    name = "Kecamatan",
    guide = guide_legend(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      label.hjust = 0.1,
      keywidth = 0.2,
      keyheight = 0.5
    )
  ) +
  coord_map() +
  theme_void() +
  theme(
    title = element_text(face = "bold"),
    legend.position = "bottom"
  ) # pindah bawah
