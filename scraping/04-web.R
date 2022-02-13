read_html("https://onepiece.fandom.com/wiki/Chapter_2") %>% {
  tibble(
    key = html_nodes(., ".pi-data-label") %>% html_text(),
    value = html_nodes(., ".pi-item-spacing .pi-font") %>% html_text()
  )
}

html <- read_html("http://aviation.bmkg.go.id/web/station.php?c=ID&pn=0")


df <- html %>% {
  tibble(
    no = html_nodes(., "td:nth-child(1)") %>% html_text(),
    ICAO = html_nodes(., "td:nth-child(2)") %>% html_text()
  )
}

df <- df[-101, ]


df <- cbind(
  df,
  html %>% {
    tibble(
      WMO = html_nodes(., "td:nth-child(3)") %>% html_text(),
      IATA = html_nodes(., "td:nth-child(4)") %>% html_text(),
      nama = html_nodes(., "td:nth-child(5)") %>% html_text(),
      lintang = html_nodes(., "td:nth-child(6)") %>% html_text(),
      bujur = html_nodes(., "td:nth-child(7)") %>% html_text(),
      ketinggian = html_nodes(., "td:nth-child(8)") %>% html_text()
    )
  }
)

df



library(rvest)
library(furrr)
library(tidyverse)


read_html("https://id.wikipedia.org/wiki/Provinsi_di_Indonesia") %>% {
  tibble(
    value = html_nodes(., ".tright+ table td+ td") %>% html_text()
  )
} -> data

read_html("https://id.wikipedia.org/wiki/Provinsi_di_Indonesia") %>% {
  tibble(
    value = html_nodes(., "table:nth-child(14) td:nth-child(1)") %>% html_text()
  )
} -> namakolom

namakolom <- as.vector(namakolom$value)


# lihat data
head(data)
namakolom

# clean data
# hapus baris tidak penting
i <- seq(1, nrow(data), by = 2)
data <- as.matrix(data[-i, ])

# hapus \n
data <- str_remove_all(data, "\n")
head(data)

# resize
data <- matrix(data, 34, 21, byrow = T)

# beri nama kolom
namakolom <- str_remove_all(namakolom, "\n")
colnames(data) <- namakolom

# Hasil
data <- as.data.frame(data)
view(data)
glimpse(data)
