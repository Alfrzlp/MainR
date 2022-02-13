library(rgdal)

indo <- readOGR(dsn = "D:/Datasets/SHP Indonesia", layer = "prov")
summary(indo@data)

library(ggplot2)
indonesia <- fortify(indo, region = "NAME_1")

library(dplyr)
library(janitor)
dataindo <- indo@data
dataindo <- clean_names(dataindo)

dataindo$id <- 1:34
indonesia <- indonesia %>% mutate(id = as.integer(id))
library(plyr)
indonesia <- join(indonesia, dataindo, by = "id")

glimpse(indonesia)

library(ggplot2)
ggplot(indonesia, aes(x = long, y = lat)) +
  geom_map(
    map = indonesia, aes(map_id = id),
    color = "white", size = 0.5
  )

head(indonesia)
dataindo
