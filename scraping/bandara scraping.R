library(rvest)
library(tidyverse)

html = read_html("http://aviation.bmkg.go.id/web/station.php?c=ID&pn=0")
html2 = read_html("http://aviation.bmkg.go.id/web/station.php?c=ID&pn=1")

df = html %>% {
  tibble(
    no = html_nodes(.,"p+ table td:nth-child(1)") %>% html_text(),
    ICAO = html_nodes(.,"p+ table td:nth-child(2)") %>% html_text(),
    WMO = html_nodes(.,"p+ table td:nth-child(3)") %>% html_text(),
    IATA = html_nodes(.,"p+ table td:nth-child(4)") %>% html_text(),
    nama = html_nodes(.,"p+ table td:nth-child(5)") %>% html_text(),
    lintang = html_nodes(.,"p+ table td:nth-child(6)") %>% html_text(),
    bujur = html_nodes(.,"p+ table td:nth-child(7)") %>% html_text(),
    ketinggian = html_nodes(.,"p+ table td:nth-child(8)") %>% html_text()
  )
}
df


df2 = html2 %>% {
  tibble(
    no = html_nodes(.,"p+ table td:nth-child(1)") %>% html_text(),
    ICAO = html_nodes(.,"p+ table td:nth-child(2)") %>% html_text(),
    WMO = html_nodes(.,"p+ table td:nth-child(3)") %>% html_text(),
    IATA = html_nodes(.,"p+ table td:nth-child(4)") %>% html_text(),
    nama = html_nodes(.,"p+ table td:nth-child(5)") %>% html_text(),
    lintang = html_nodes(.,"p+ table td:nth-child(6)") %>% html_text(),
    bujur = html_nodes(.,"p+ table td:nth-child(7)") %>% html_text(),
    ketinggian = html_nodes(.,"p+ table td:nth-child(8)") %>% html_text()
  )
}
df2

df = rbind(df, df2)
df

df %>% 
  filter(str_detect(ICAO, "\\d"))

df = df %>% 
  separate(nama, c("nama_bandara", "kota") , sep = " - ") %>% 
  mutate(kota = if_else(is.na(kota), nama_bandara, kota)) %>% 
  select(-1) %>% 
  type_convert()


df[df == ""] = NA
df

df

# fungsi
DMS2latlong = function(y){
  ifelse(substr(y, 1, 1) == "S", p <- -1, p <-  1)
  x = trimws(gsub("\\D", " ", y))
  x = as.numeric(str_split(x, "\\s")[[1]])
  x = p*(x[1] + (x[2]/60) + (x[3]/3600))
  return(x)
}

df = df %>% 
  mutate(lintang = sapply(df$lintang, DMS2lat, USE.NAMES = F),
         bujur = sapply(df$bujur, DMS2lat, USE.NAMES = F)) %>% 
  as.data.frame()

df
