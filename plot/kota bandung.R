library(geojsonio)

bandung = geojson_read('E:/kotabandung.geojson', what = 'sp')
plot(bandung)

# Mengubah menjadi DataFrame ----------------------------------------------
library(ggplot2)

df <- fortify(bandung, region = 'NAME_3')
head(df)


# Menambahkan Data --------------------------------------------------------
library(readxl)

data_ikp <- read_xlsx("C:/Users/Ridson Alfarizal/Downloads/IKP KotBan.xlsx")
data_ikp

# Cek Nama kec di Xlsx apakah sudah sama dengan data peta,
# baik spasi, huruf besar kecil
unique(df$id)

# Join df
df <- df %>% 
  left_join(data_ikp, by = c('id' = 'Kecamatan')) 



# Visualisasi -------------------------------------------------------------
head(df)

ggplot(df) +
  geom_polygon(
    aes(x = long, y = lat, fill = IKP, group=group), color="white"
  ) +
  theme_minimal() +
  coord_map() +
  labs(
    title = 'Kota Bandung',
    x = NULL, y = NULL,
    caption = expression(italic("Sumber Data : bps.go.id"))
  ) 



# Interaktif --------------------------------------------------------------
library(leaflet)

bandung@data <- 
  bandung@data %>% 
  left_join(data_ikp, by = c('NAME_3' = 'Kecamatan')) 
  
  
pal = colorFactor("viridis", domain = bandung$IKP)

mytext = paste(
  "<b>Kecamatan :</b>",bandung$NAME_3,"<br/>",
  "<b>IKP &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; :</b>",bandung$IKP,"<br/>"
) %>% 
  lapply(htmltools::HTML) 



leaflet(bandung) %>% 
  setView(lng = 107.65260717856989,
          lat = -6.904715943052474, zoom = 12) %>% 
  # addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolygons(
    weight = 2,
    color = "white", # warna garis
    fillColor = ~ pal(bandung$IKP), #memberi warna polygon
    fillOpacity = 1, #ketebalan warna polygon
    label = mytext,
    labelOptions = labelOptions(
    style = list("font-weight"="normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto",
                  ),
    highlightOptions = highlightOptions(color = "white", #warna garis ketika cursor disitu
                                        weight = 2,
                                        bringToFront = T)
  )
  addLegend("bottomright", pal = pal, values = ~IKP,
            title = "IKP",
            opacity = 1
  )
