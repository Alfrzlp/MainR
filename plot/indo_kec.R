library(rgdal)
library(rgeos)
library(ggplot2)
library(maptools)

indo = readOGR(dsn = "D:/Datasets/gadm36_IDN_shp", layer = "gadm36_IDN_1", stringsAsFactors = F)

indonesia = fortify(indo, region = "NAME_2")
dataindo = indo@data
colnames(dataindo)[7] = "id"

library(dplyr)
indonesia = left_join(indonesia, dataindo, by="id")

library(ggplot2)
ggplot(indonesia, aes(x=long, y=lat)) + 
  geom_map(map=indonesia, aes(map_id=id), color="white", size=0.5)

jatim = indonesia %>% filter(NAME_1 == "Jawa Timur")
jatim

ggplot(jatim, aes(x=long, y=lat)) + 
  geom_map(map=jatim, aes(map_id=id, fill=id), color="white", size=0.5)


jatim
