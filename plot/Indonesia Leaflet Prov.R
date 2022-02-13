library(tidyverse)
library(rgdal)

indo <- readOGR(dsn = "D:/Datasets/SHP Indonesia", layer = "prov")
indonesia <- fortify(indo, region = "NAME_1")
head(indonesia)

indonesia %>%
  group_by(id) %>%
  summarise(n()) %>%
  as.data.frame()

df <- readxl::read_xlsx("D:/tpt rls.xlsx",
  col_names = c("prov", "tpt", "rls")
) %>%
  drop_na() %>%
  filter(prov != "INDONESIA") %>%
  mutate(
    rls = str_replace_all(rls, ",", "."),
    prov = str_to_title(prov),
    prov = if_else(prov == "Dki Jakarta", "DKI  Jakarta", prov)
  ) %>%
  type_convert()


id <- indonesia %>%
  left_join(df, by = c("id" = "prov"))



# ggplot way ==============
my_caption <- expression(italic("Sumber Data : bps.go.id"))

ggplot(id, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = rls, group = group), col = "white") +
  theme_void() +
  scale_fill_viridis_c(direction = -1, option = "inferno") +
  labs(
    title = "Rata-rata Lama Sekolah Penduduk Indonesia Tahun 2020",
    fill = "RLS", caption = my_caption
  )

ggsave("D:/rls.png", width = 20, height = 15, units = "cm")


# leaflet way =============
library(leaflet)
library(mapview)

indo@data <- indo@data %>%
  left_join(df, by = c("NAME_1" = "prov"))

hist(indo@data$rls)
indo@data %>%
  filter(rls == max(rls))


pal <- colorNumeric(palette = "YlOrRd", domain = indo$rls)
pal2 <- colorBin("YlOrRd",
  domain = indo$rls,
  bins = c(0, 7, 8, 9, 10, 20)
)



leaflet(indo) %>%
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  setView(lat = -0.5705987379108618, lng = 118.65416707600103, zoom = 5) %>%
  addPolygons(
    weight = 2,
    color = "black", # warna garis
    fillColor = ~ pal(indo$rls),
    fillOpacity = 0.9, # ketebalan warna
    popup = paste0(
      "<b>Provinsi  :</b>", indo$NAME_1,
      "<br>",
      "<b>RLS  :</b>", indo$rls
    )
  ) %>%
  addLegend(
    pal = pal, values = indo$rls,
    title = "Rata-rata Lama Sekolah",
    position = "topright"
  ) %>%
  addCircleMarkers(
    lng = 106.828303, lat = -6.223996,
    popup = paste0(
      "<b>Provinsi  :</b>", "Jakarta",
      "<br>",
      "<b>RLS\t:</b>", 11.17
    ),
    popupOptions = popupOptions(closeButton = F, closeOnClick = F)
  )

addMarkers(
  lng = 106.828303, lat = -6.223996,
  label = paste0(
    "Provinsi\t:", "Jakarta\n",
    "RLS\t:", 11.17
  ),
  labelOptions = labelOptions(noHide = T, textsize = "15px")
)

# mapshot(map, file = 'D:/rls2.png')










# ggplot way with some args ==========================
df <- readxl::read_xlsx("D:/presentase_kemiskinan.xlsx") %>%
  type.convert() %>%
  mutate(
    prov = str_to_title(prov),
    prov = if_else(prov == "Dki Jakarta", "DKI Jakarta", prov),
    prov = if_else(prov == "Di Yogyakarta", "DI Yogyakarta", prov)
  ) %>%
  janitor::clean_names()

df

indoensia <- df %>%
  right_join(indonesia, by = c("prov" = "id"))

head(indoensia)


# yang max dan min
point <- indonesia %>%
  # filter(maret_2020 == min(maret_2020 ) | maret_2020 == max(maret_2020 )) %>%
  group_by(prov) %>%
  summarise(
    long = median(long), lat = median(lat),
    ppmiskin = mean(maret_2020)
  ) %>%
  as.data.frame() %>%
  top_n(5, wt = ppmiskin)

point


# cividis, mako, rocket
library(ggrepel)
map <- map_data("world")


my_caption <- expression(italic("Sumber Data : bps.go.id"), size = 13)

ggplot() +
  geom_polygon(data = map, aes(long, lat, group = group), fill = "white") +
  xlim(95, 141) +
  ylim(-11, 8) +
  geom_polygon(data = indonesia, aes(
    x = long, y = lat,
    fill = maret_2020, group = group
  ), col = "black") +
  theme_bw() +
  scale_fill_viridis_c(direction = -1, option = "inferno") +
  labs(
    title = "Presentase Penduduk Miskin Indonesia",
    subtitle = "Maret 2020",
    fill = "", caption = my_caption
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    panel.background = element_rect(fill = "lightblue")
  ) +
  geom_point(
    data = point,
    aes(x = long, y = lat), col = "lightgreen", size = 3
  ) +
  geom_label_repel(
    data = point,
    aes(x = long, y = lat, label = paste(prov, ":", ppmiskin)),
    size = 4, vjust = -1, hjust = -1
  )
# vertikal + bawah
# horizontal - kanan

# save
ggsave("D:/pmiskin.png", width = 20, height = 15, units = "cm")


ggplot(df, aes(x = tpt, y = september_2020)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    y = "Presentase Penduduk Miskin",
    x = "Tingkat pengangguran Terbuka"
  )
