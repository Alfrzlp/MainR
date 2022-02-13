library(doSNOW)
library(foreach)
library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
library(jsonlite)
library(magick)
library(lubridate)

# daftar lokasi file json
file <- list.files("D:/Datasets/Jakarta Traffic", full.names = T)

# fungsi olah data mentah
# referensi dari package HereR
resume <- function(df) {
  .line_from_point_list <- function(point_list) {
    coords <- strsplit(point_list, ",")
    lng <- as.numeric(sapply(coords, function(x) x[2]))
    lat <- as.numeric(sapply(coords, function(x) x[1]))
    sf::st_linestring(cbind(lng, lat))
  }

  data1 <- cbind(
    df$FI[[1]]["TMC"]$TMC,
    df$FI[[1]]["CF"]$CF %>% bind_rows() %>% select(TY, SP, FF, JF, CN)
  )

  geometry <- lapply(df$FI[[1]]["SHP"]$SHP, function(shp) {
    lines <- lapply(shp$value, function(point_list) {
      .line_from_point_list(strsplit(point_list, " ")[[1]])
    })
    sf::st_multilinestring(lines)
  })

  data1$geometry <- geometry
  data1$FC <- sapply(df$FI[[1]]["SHP"]$SHP, function(a) a[1, 2])
  return(data1)
}


# mengolah data mentah menjadi data.frame. input = lokasi file
get_resume <- function(lokasi, FUN = resume) {
  start <- Sys.time()
  flow <- fromJSON(lokasi)
  flow <- flow$RWS$RW[[1]]$FIS

  cores <- parallel::detectCores()
  cl <- makeSOCKcluster(cores - 1)
  registerDoSNOW(cl)

  df <- resume(flow[[1]])
  # iterasi secara pararel agar cepat
  df <- foreach(i = 2:length(flow), .combine = rbind, .packages = c("dplyr", "stringr", "sf"), .inorder = F) %dopar% {
    FUN(flow[[i]])
  }
  stopCluster(cl)


  df <- sf::st_set_crs(sf::st_as_sf(df), 4326)
  # buat group
  df <- df %>%
    mutate(id = paste0(PC, QD), .keep = "unused", .before = "DE") %>%
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
  print(Sys.time() - start)
  return(df)
}

# fungsi untuk plot gambar
plot_leaflet <- function(df, lng = 106.828303, lat = -6.223996, zoom = 11) {
  map <- df %>%
    arrange(group) %>%
    leaflet() %>%
    addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
    setView(lng = lng, lat = lat, zoom = zoom) %>%
    # fitBounds(106.559667, -6.077811, 107.146117, -6.665355) %>%
    addPolylines(color = ~col, opacity = 2, weight = ~ 5 / FC)

  return(map)
}



# Kita test fungsinya
data <- get_resume(file[1])
map <- plot_leaflet(data)
map


# plot dan save png
for (j in 1:length(file)) {
  df <- get_resume(file[j])
  map <- plot_leaflet(df)
  # save gambar langsung ke folder img jkt
  # dengan nama yang sama dengan file json nya
  alamat <- file.path(getwd(), "Main R/traffic/img jkt")
  alamat <- file.path(alamat, gsub("json", "png", list.files("D:/Datasets/Jakarta Traffic")[j]))
  mapshot(map, file = alamat)
}



# Proses menambah informasi pd gambar dan menjadikan gif

# ambil semua lokasi file gambar yg akan dibaca
all_loc <- list.files("Main R/traffic/img jkt", full.names = T)

hari <- c("S a b t u", "M i n g g u", "S e n i n")
waktu <- ymd_hms("2021-02-20 00:00:00")
img_list <- list()
j <- 0

for (i in 1:length(all_loc)) {
  # tujuannya untuk merubah 6:30 menjadi 06:30
  # biar bagus aja tampilannya wkwk
  jam <- ifelse(nchar(hour(waktu)) == 1, paste0("0", hour(waktu)), hour(waktu))
  menit <- ifelse(nchar(minute(waktu)) == 1, paste0("0", minute(waktu)), minute(waktu))

  # baca gambar
  image <- image_read(all_loc[i])
  # write time
  image_text <- image_annotate(image, paste0(jam, ":", menit),
    color = "white", size = 40,
    weight = 700, location = paste0("+", j %% 912 + 10, "+630")
  )
  j <- j + 19
  # write day
  # karena dimulai hari sabtu
  image_text <- image_annotate(image_text, hari[day(waktu) %% 20 + 1],
    color = "gray50", size = 20,
    location = "+10+670", weight = 700
  )
  # write date
  date <- paste(day(waktu), "Februari", year(waktu))
  image_text <- image_annotate(image_text, date,
    color = "gray", size = 20,
    location = "+10+690", weight = 700
  )
  # tambah 30 menit
  waktu <- waktu + dminutes(30)
  # tambahkan ke list
  img_list <- append(img_list, image_text)
}


imgs <- image_join(img_list)
gif <- image_animate(imgs, fps = 5, loop = 1)

# simpan
image_write_gif(gif, "D:/jakarta.gif")
