library(tidyverse)
setwd("C:/Users/Ridson Alfarizal/Documents/")
setwd("Main R/personal-project-master/flowmap")

rute <- read.delim("(D:/Datasets/apapun/routes.dat.txt)",
  sep = ",",
  header = F
)
bandara <- read.delim("(D:/Datasets/apapun/airports.dat.txt)",
  sep = ",",
  header = F
)

rute
bandara

# bandara
# Airport ID	Unique OpenFlights identifier for this airport.
# Name	Name of airport. May or may not contain the City name.
# City	Main city served by airport. May be spelled differently from Name.
# Country	Country or territory where airport is located. See Countries to cross-reference to ISO 3166-1 codes.
# IATA	3-letter IATA code. Null if not assigned/unknown.
# ICAO	4-letter ICAO code.
# Null if not assigned.
# Latitude	Decimal degrees, usually to six significant digits. Negative is South, positive is North.
# Longitude	Decimal degrees, usually to six significant digits. Negative is West, positive is East.
# Altitude	In feet.
# Timezone	Hours offset from UTC. Fractional hours are expressed as decimals, eg. India is 5.5.
# DST	Daylight savings time. One of E (Europe), A (US/Canada), S (South America), O (Australia), Z (New Zealand), N (None) or U (Unknown). See also: Help: Time
# Tz database time zone	Timezone in "tz" (Olson) format, eg. "America/Los_Angeles".
# Type	Type of the airport. Value "airport" for air terminals, "station" for train stations, "port" for ferry terminals and "unknown" if not known. In airports.csv, only type=airport is included.


# rute
# Airline	2-letter (IATA) or 3-letter (ICAO) code of the airline.
# Airline ID	Unique OpenFlights identifier for airline (see Airline).
# Source airport	3-letter (IATA) or 4-letter (ICAO) code of the source airport.
# Source airport ID	Unique OpenFlights identifier for source airport (see Airport)
# Destination airport	3-letter (IATA) or 4-letter (ICAO) code of the destination airport.
# Destination airport ID	Unique OpenFlights identifier for destination airport (see Airport)
# Codeshare	"Y" if this flight is a codeshare (that is, not operated by Airline, but another carrier), empty otherwise.
# Stops	Number of stops on this flight ("0" for direct)
# Equipment	3-letter codes for plane type(s) generally used on this flight, separated by spaces


colnames(bandara) <- c(
  "id", "nama", "kota", "negara", "IATA", "ICAO", "lat", "long",
  "altitude", "bedadariUTC", "DST"
)
colnames(rute) <- c(
  "kode", "ID", "asal_bandara", "ID_asal_bandara", "tujuan_bandara", "ID_tujuan_bandara",
  "Codeshare", "jumlah_pemberhentian", "plane_type"
)

bandara <- bandara %>%
  select(nama, kota, negara, IATA, ICAO, lat, long) %>%
  filter(negara == "Indonesia")

rute <- rute %>%
  select(-c(plane_type, Codeshare, kode, ID))

rute %>%
  filter(nchar(asal_bandara) == 4)

bandara
rute


# membuat peta indonesia
head(indonesia)

ggplot(indonesia) +
  geom_map(
    map = indonesia, aes(x = long, y = lat, map_id = id),
    color = "white", size = 0.5
  )

ggplot(indonesia) +
  geom_polygon(aes(x = long, y = lat, group = group),
    color = "white", fill = "black", size = 0.07
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    # hapus garis2
    panel.grid = element_blank(),
    # warna luar
    plot.background = element_rect(fill = "black"),
    # hapus angka x y
    axis.text = element_blank()
  )

rute %>%
  left_join(bandara, by = c("asal_bandara" = "IATA")) %>%
  drop_na(negara)


rute_id <- rute %>%
  left_join(bandara, by = c("asal_bandara" = "IATA")) %>%
  drop_na(lat, long) %>%
  select(-jumlah_pemberhentian, -ICAO, -negara, -ID_asal_bandara, -ID_tujuan_bandara) %>%
  rename(
    nama_asal = nama, kota_asal = kota,
    lat0 = lat, long0 = long
  ) %>%
  left_join(bandara, by = c("tujuan_bandara" = "IATA")) %>%
  drop_na(lat, long) %>%
  rename(
    nama_tujuan = nama, kota_tujuan = kota,
    lat1 = lat, long1 = long
  ) %>%
  select(-c(negara, ICAO))


# file geojson --------------------------------------------------------
library(geojsonio)
spdf <- geojson_read("data/rbi/id_prov.geojson", what = "sp")
indo <- fortify(spdf, region = "NAME_1")
head(indo)

ggplot(indo) +
  geom_polygon(aes(x = long, y = lat, group = group),
    color = "white", fill = "black", size = 0.05
  ) +
  geom_segment(
    data = rute_id, aes(
      x = long0, y = lat0,
      xend = long1, yend = lat1
    ),
    color = "green", lineend = "round", alpha = 0.8
  ) +
  geom_point(data = rute_id, aes(x = long0, y = lat0), color = "yellow") +
  geom_point(data = rute_id, aes(x = long1, y = lat1), color = "yellow") +
  theme(
    panel.background = element_rect(fill = "black"),
    # hapus garis2
    panel.grid = element_blank(),
    # warna luar
    plot.background = element_rect(fill = "black"),
    # hapus angka x y
    axis.text = element_blank()
  )


# alt -



# transjakarta ========================================================
setwd("Main R/personal-project-master/flowmap")


#  tj
tj <- read_rds("data/tj/TJ.rds")
# rute
tj_rute <- read_rds("data/tj/sp_tj_rute.rds")
tj_rute <- tj_rute %>%
  dplyr::filter(isHidden == F)

tj_rute <- read_rds("data/tj/routesDF.rds")
# halte
tj_halte <- read_rds("data/tj/tb_tj_halte.rds")


library(geojsonio)
library(hrbrthemes)
spdf <- geojson_read("data/rbi/KotaJadetabek.geojson", what = "sp")
jakarta <- fortify(spdf, region = "NAME_2") %>%
  filter(str_detect(id, "Jakarta"))

head(jakarta)


# pakai polygon
ggplot(jakarta) +
  geom_polygon(aes(x = long, y = lat, group = group),
    color = "grey", fill = NA, size = 0.1, linetype = "dashed"
  ) +
  geom_sf(data = tj_rute, col = "steelblue2", size = 0.1, alpha = 0.5, show.legend = FALSE, lineend = "round") +
  # geom_point(data = tj_halte, aes(x = lon, y = lat), color = "white", size = 0.7, alpha = 0.1) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    # hapus garis2
    panel.grid = element_blank(),
    # warna luar
    plot.background = element_rect(fill = "black"),
    # hapus angka x y
    axis.text = element_blank(),
    plot.margin = grid::unit(c(3, 3, 3, 3), "mm")
  )



tj_rute$geometry[[1]]

library(sf)
dki <- st_read("data/rbi/KotaJadetabek.geojson") %>%
  filter(str_detect(NAME_1, "Jakarta"))

ggplot(dki) +
  geom_sf(data = dki, col = "grey", fill = "black", size = 0.1, linetype = "dashed") +
  # geom_sf(data = tj_rute, col = "white", size = 0.05, alpha = 0.5, show.legend = FALSE) +
  geom_sf(data = tj_rute, col = "steelblue2", size = 0.1, show.legend = FALSE) +
  # geom_point(data = tj_halte, aes(x = lon, y = lat), color = "white", size = 0.7, alpha = 1, shape = 19) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    # hapus garis2
    panel.grid = element_blank(),
    # warna luar
    plot.background = element_rect(fill = "black"),
    # hapus angka x y
    axis.text = element_blank(),
    plot.margin = grid::unit(c(3, 3, 3, 3), "mm")
  )
