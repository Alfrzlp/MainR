saguling <- readxl::read_xlsx("D:/Downloads/Copy of [RISET 2] KBB_Saguling.xlsx")
saguling
saguling <- saguling %>%
  dplyr::filter(desa %in% c("002", "004", "006")) %>%
  mutate(
    desa = case_when(
      desa == "006" ~ "Saguling",
      desa == "004" ~ "Cipangeran",
      desa == "002" ~ "Cikande",
    )
  )
saguling

library(geojsonio)

saguling <- geojson_read("D:/Downloads/pointsg.geojson",
  what = "sp", stringsAsFactors = F
)
saguling %>%
  glimpse()


read.csv("D:/Downloads/pointsg.csv") %>%
  janitor::clean_names() %>%
  rename(desa = kec_sagul) %>%
  dplyr::filter(id %in% c(31, 54, 55, 65, 71, 28, 8, 18)) %>%
  # dplyr::filter(desa %in% c('2', '4', '6')) %>%
  # mutate(
  #   desa = case_when(
  #     desa == '6'~'Saguling',
  #     desa == '4'~'Cipangeran',
  #     desa == '2'~'Cikande',
  #   )
  # ) %>%
  extract(wkt,
    into = c("long", "lat"),
    regex = "POINT \\((-?\\d*?\\.\\d*|-?\\d+) (-?\\d*?\\.\\d*|-?\\d+)\\)"
  ) %>%
  write.csv("D:/Downloads/point_desa_sg.csv", row.names = F)



library(sp)
longitude <- c(107.3923891, 107.39214637)
latitude <- c(-6.8976564, -6.89781698)
lonlat <- cbind(longitude, latitude)


s <- "Latifah -6.907092514447868 107.37399104051292
Suryani -6.907630506902933 107.368298554793
Nunung -6.908078477717936 107.36863919533789
Sari -6.902362857945263 107.36663273535669
Arif -6.889751614071429 107.37094941549003
Atika -6.88825502526015 107.3754945024848
Didin -6.888189939782023 107.37574193626642"

dat <-
  read.table(textConnection(s), header = F) %>%
  `colnames<-`(c("nama", "long", "lat"))

dat
pts <- SpatialPoints(dat[, -1])
class(pts)
showDefault(pts)


pts <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
pts

plot(pts)
class(pts)

df <- data.frame(nama = dat$nama)
df

ptsdf <- SpatialPointsDataFrame(pts, data = df)
library(rgdal)
writeOGR(ptsdf, "E:/pointku.geojson", layer = "pointku", driver = "GeoJSON")




# -------------------------------------------------------------------------

pch <- readxl::read_xlsx('D:/__SEMESTER 6/_PKL/hasil/VPKL61_R2_PENCACAHAN_results.json.xlsx')
pch %>% 
  dplyr::filter(a203b == '040') %>% 
  distinct(a102, .keep_all = T) %>% 
  dim()


gc <- readxl::read_xlsx('D:/__SEMESTER 6/_PKL/hasil/VPKL61_R2_GROUNDCHECK_results.json.xlsx')
gc

gc %>% 
  dplyr::filter(
    b203b == '040',
    b201 == 'RIDSON ALFARIZAL PULUNGAN'
  ) %>% 
  mutate(
    no = as.numeric(substr(b106, start = 6, 7)),
    .before = record
  ) %>% 
  distinct(no) %>% 
  as.data.frame()
count()
arrange(no) %>% 
  as.data.frame() %>% 
  pull(no)

id <- saguling %>% 
  dplyr::filter(desa %in% c("002", "004", "006")) %>% 
  pull(id)

setdiff(id, no)

saguling %>% 
  dplyr::filter(desa %in% c("002", "004", "006"))



27 60
cikande 23
cipangeran 9
saguling 8
