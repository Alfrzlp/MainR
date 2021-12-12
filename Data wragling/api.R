library(plumber)
library(tidyverse)
library(nycflights13)
library(jsonlite)

# Rest api (representational state transfer API)
# Request method
# get = baca
# post = masukkan data
# put = update data
# delete

url <- "https://covid19-public.digitalservice.id/api/v1/rekapitulasi/jabar/perminggu"
rekp <- fromJSON(url)
rekp

flights
weather
airports
