library(tidyverse)
library(gtrendsR)
library(sf)


# -------------------------------------------------------------------------
banten <- st_read("D:/__Datasets/Peta/banten.geojson")
banten

nmkec <- unique(banten$nmkec)

res <- gtrendsR::gtrends(
  keyword = "info lowongan pekerjaan",
  geo = "ID",
  time = 'today 12-m'
)

glimpse(res)
sort(nmkec)


#  -----------------------------------------------------------------------
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

# pak::pak(c("osmdata", "ggmap"))
head(available_features())
head(available_tags("amenity"))
head(available_tags("shop"))

q <- getbb("Banten") %>%
  opq() %>%
  add_osm_feature("amenity", "cinema")
str(q)

cinema <- osmdata_sf(q)
cinema


#our background map
mad_map <- get_map(getbb("Banten"), maptype = "toner-background")

#final map
ggmap(mad_map)+
  geom_sf(data = cinema$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")
