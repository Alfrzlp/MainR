library(tidyverse)
library(patchwork)
library(janitor)
library(ggrepel)
library(usethis)
library(lubridate)
library(colorspace)
library(scales)
library(kableExtra)
library(knitr)
library(sf)

use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=1/2020-01-01_performance_mobile_tiles.zip",
  destdir = "D:/Datasets/ookla"
)
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=2/2020-04-01_performance_mobile_tiles.zip",
  destdir = "D:/Datasets/ookla"
)
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=3/2020-07-01_performance_mobile_tiles.zip",
  destdir = "D:/Datasets/ookla"
)
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=4/2020-10-01_performance_mobile_tiles.zip",
  destdir = "D:/Datasets/ookla"
)

use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2021/quarter=1/2021-01-01_performance_mobile_tiles.zip",
  destdir = "D:/Datasets/ookla"
)




jatim_bbox <- read_sf("D:/Datasets/gadm36_IDN_shp/gadm36_IDN_2.shp")
jatim_bbox
jatim_bbox <- jatim_bbox %>%
  dplyr::filter(NAME_1 == "Jawa Timur") %>%
  st_transform(4326) %>%
  clean_names()


ggplot(jatim_bbox) +
  geom_sf(color = "grey", fill = "black", lwd = 0.08) +
  # geom_text_repel(data = uk_cities,
  #                 aes(label = name_1, geometry = geometry),
  #                 family = "sans",
  #                 color = 'gray',
  #                 size = 2.2,
  #                 stat = "sf_coordinates",
  #                 min.segment.length = 2) +
  labs(
    title = "Kecepatan Internet Selular | Q4 2020",
    subtitle = ""
  ) +
  theme_minimal()
theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank()
)


# and then read in those downloaded files
mobile_tiles_q1 <- read_sf("D:/Datasets/ookla/2020-01-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  st_crop(jatim_bbox)
mobile_tiles_q2 <- read_sf("D:/Datasets/ookla/2020-04-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  st_crop(jatim_bbox)
mobile_tiles_q3 <- read_sf("D:/Datasets/ookla/2020-07-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  st_crop(jatim_bbox)
mobile_tiles_q4 <- read_sf("D:/Datasets/ookla/2020-10-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  st_crop(jatim_bbox)


purple <- colorspace::lighten("#A244DA", 0.5)

ggplot(jatim_bbox) +
  geom_sf(color = "grey", fill = "black", lwd = 0.08) +
  geom_sf(data = mobile_tiles_q4, fill = purple, color = NA) +
  labs(
    title = "Kecepatan Internet Selular | Q4 2020",
    subtitle = ""
  ) +
  theme_minimal()
theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank()
)
