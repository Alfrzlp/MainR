library(hereR)
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(jsonlite)

set_key("MI1Puf0qa1RVDJDTWyNmiPZabg2IqRPPu5CEm6DRsHs")

# Real-time traffic flow
flow2 <- flow(
  aoi = polys
)

head(flow2)

flow2 <- flow2 %>%
  mutate(
    group = cut(JF, c(-1, 1, 3, 4, 8, 11), c(1, 2, 3, 4, 5)),
    col = case_when(
      group == 1 ~ "#034732",
      group == 2 ~ "#008148",
      group == 3 ~ "#069E2D",
      group == 4 ~ "#F5BB00",
      group == 5 ~ "#FB5012"
    )
  )

fc <- flow %>%
  select(id, FC)

flow2 <- flow2 %>%
  mutate(id = paste0(PC, QD))




data.frame(x = 1:10) %>%
  mutate(g = cut(x, c(-1, 1, 2, 4, 8, 11), c(1, 2, 3, 4, 5)))


hist(flow2$JF)



library(indonesia)
jakarta <- id_map("indonesia", "provinsi")
jakarta <- jakarta %>%
  filter(nama_provinsi == "DKI Jakarta")


flow <- flow(
  aoi = jakarta
)

head(flow)
dim(flow)


flow <- flow %>%
  mutate(
    group = cut(JF, c(-1, 1, 3, 4, 8, 11), c(1, 2, 3, 4, 5)),
    col = case_when(
      group == 1 ~ "#034732",
      group == 2 ~ "#008148",
      group == 3 ~ "#069E2D",
      group == 4 ~ "#F5BB00",
      group == 5 ~ "#FB5012"
    )
  )


flow2 %>%
  mutate(LE = if_else(LE > 4, 6, LE)) %>%
  arrange(group) %>%
  leaflet() %>%
  addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
  setView(lng = 106.828303, lat = -6.223996, 11) %>%
  # fitBounds(106.559667, -6.077811, 107.146117, -6.665355) %>%
  addPolylines(color = ~col, opacity = 2, weight = ~LE)

map


max(flow2$LE)
flow2 <- flow2 %>%
  mutate(LE = if_else(LE > 2.5, 2, 5, LE))


flow2 %>%
  filter(str_detect(tolower(DE), "cililitan")) %>%
  view()


round(flow$LE, 2)
saveWidget(map, "D:/jkt.html")






long0 <- 106.559667
lat0 <- -6.077811
long1 <- 107.028342
lat1 <- -6.407725
# -6.407725, 107.028342

dat <- data.frame(rbind(
  c(106.559667, -6.077811),
  c(106.559667, -6.665355),
  c(107.146117, -6.665355),
  c(107.146117, -6.077811)
))
colnames(dat) <- c("long", "lat")

xys <- st_as_sf(dat, coords = c("long", "lat"))

polys <- xys %>%
  dplyr::summarise() %>%
  st_cast("POLYGON") %>%
  st_convex_hull() %>% # biar mulus
  st_set_crs(4326)
polys$geometry[[1]]


df1 <- df1 %>%
  mutate(id = paste0(PC, QD))
xys <- st_as_sf(df1, coords = c("long", "lat"))
polys <- xys %>%
  dplyr::group_by(id) %>%
  dplyr::summarise() %>%
  st_cast("MULTILINESTRING") %>%
  st_set_crs(4326)

head(polys)
dim(polys)
df1 <- df1 %>%
  distinct(id, .keep_all = T) %>%
  select(-long, -lat)



hist(polys$JF)

polys <- polys %>%
  mutate(
    group = cut(JF, c(-1, 1, 2, 3, 8, 11), c(1, 2, 3, 4, 5)),
    col = case_when(
      group == 1 ~ "#034732",
      group == 2 ~ "#008148",
      group == 3 ~ "#069E2D",
      group == 4 ~ "#F5BB00",
      group == 5 ~ "#FB5012"
    )
  )

polys %>%
  arrange(group) %>%
  leaflet() %>%
  addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
  setView(lng = 106.828303, lat = -6.223996, 11) %>%
  # fitBounds(106.559667, -6.077811, 107.146117, -6.665355) %>%
  addPolylines(color = ~col, opacity = 2, weight = ~ 5 / FC)

max(polys$FC)
