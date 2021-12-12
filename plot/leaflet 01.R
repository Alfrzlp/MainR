library(dplyr)
library(leaflet)
library(rgdal)

indo = readOGR(dsn = "D:/Datasets/SHP Indonesia", layer = "prov")

#indo$col = rainbow(nrow(indo@data))

pal2 = colorBin("RdYlGn", domain = indo$KODE,
                bins = c(0,10,20,30,40,50,60,70,80,90,100))
pal = colorNumeric(palette = "viridis", domain = indo$KODE)
pal3 = colorQuantile(palette = "viridis", domain = indo$KODE)

mytext = paste(
  "Provinsi :",indo$NAME_1,"<br/>",
  "Kode :", indo$KODE,"<br/>"
) %>% lapply(htmltools::HTML)

m = leaflet(indo) %>% addTiles() %>% 
  setView(lng = 118, lat=-2.5, 4) %>% 
    addPolygons(weight = 2,
              #color = "black", #warna garis
              fillColor = ~pal2(indo$KODE),
              fillOpacity = 0.9, #ketebalan warna
              label = mytext,
              labelOptions = labelOptions(
                style = list("font-weight"="normal", padding = "3px 8px"),
                textsize = "13px",
               # direction = "auto",
              ),
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = T)) %>% 
  addLegend(pal = pal2, values = indo$KODE,
            title = "kode provinsi",
            position = "bottomleft")
m

#library(htmlwidgets)
#saveWidget(m, file = "D:/Datasets/leaflet.html")
