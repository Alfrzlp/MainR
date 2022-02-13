library(leaflet)
library(rgdal)
library(spdplyr)

id <- readOGR(dsn = "D:/_Datasets/gadm36_IDN_shp", layer = "gadm36_IDN_4")
id$NAME_2
bb <- id[id$NAME_2 %in% c("Bandung Barat"), ]
pw <- id[id$NAME_2 %in% c("Purwakarta"), ]

par(mar = c(0, 0, 0, 0))
plot(bb)
plot(pw)

bb@data
class(bb)

library(raster)
shapefile(bb, filename = "E:/bbpw/bbpw_kec")

lon <- c(107.16770371007445, 107.78843124913695, 107.78843124913695, 107.16770371007445, 107.16770371007445)
lat <- c(-7.143541567965983, -7.143541567965983, -6.680017200258927, -6.680017200258927, -7.143541567965983)
lonlat <- cbind(lon, lat)

pols <- spPolygons(lonlat, crs = CRS("+proj=longlat +datum=WGS84"))
pols

ggplot(pols, aes(lon, lat)) +
  geom_polygon(color = "blue")
geom_text(label = paste(lat, "", lon))

library(geojsonio)
library(rmapshaper)
bb_geojson <- geojson_json(bb)
# Agar lebih simpel
bb_geojson <- ms_simplify(bb_geojson)

library(lawn)
view(lawn_bbox_polygon(c(107.167704, -7.143542, 107.788431, -6.680017)))
bb_geojson_clip <- ms_clip(bb_geojson,
  bbox = c(107.167704, -7.143542, 107.788431, -6.680017)
)




geojson_write(bb_geojson, file = "E:/purwakarta.geojson")
geojson_write(bb_geojson_clip, file = "E:/xampp/htdocs/Web/assest/bb_clip.geojson")



jabar <- readOGR(
  dsn = "D:/_Datasets/Jabar",
  layer = "jabar"
)
pal <- colorFactor("viridis", domain = jabar$GID_2)

mytext <- paste(
  "<strong>Kabupaten/Kota :</strong>", jabar$NAME_2, "<br/>"
) %>%
  lapply(htmltools::HTML)
mytext


m <- leaflet(jabar) %>%
  setView(
    lng = 107.61260717856989,
    lat = -6.904715943052474, zoom = 8
  ) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(
    weight = 2,
    color = "gray70", # warna garis
    fillColor = ~ pal(jabar$GID_2), # memberi warna polygon
    fillOpacity = 0.5, # ketebalan warna polygon
    label = mytext,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto",
    ),
    highlightOptions = highlightOptions(
      color = "white", # warna garis ketika cursor disitu
      weight = 2,
      bringToFront = T
    )
  )
m

gdata::read.xls("D:/_Datasets/penduduk_indonesi.xls")
