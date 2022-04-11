
loc <- 'D:/__SEMESTER 6/_PKL/PKL lanjutan/Bagi Beban Pendataan.xlsx'
gc <- readxl::read_xlsx(loc, sheet = 'GroundCheck') %>% 
  janitor::clean_names() %>% 
  rename(pcl = x6) %>% 
  extract(lat_long, c('latitude', 'longitude'), regex = '(.*),(.*)') %>% 
  drop_na()

view(gc)
head(gc)
dim(gc)
gc$latitude
gc$longitude

library(sp)

# 21.190089
measurements::conv_unit('21 11 24.32', from = "deg_min_sec", to = "dec_deg")
char2dms('21d11m24.32sN', chd='d', chm='m', chs='s') %>% as.numeric()

gc$latitude[1]
char2dms(gc$latitude[1], chd='°', chm='\'', chs='\"') %>% as.numeric()

gc <- gc %>% 
  mutate(
    no = str_sub(as.character(id), 6, 7),
    .before = id
  ) %>% 
  mutate(
    latitude = char2dms(latitude, chd='°', chm='\'', chs='\"') %>% as.numeric(),
    longitude = char2dms(longitude, chd='°', chm='\'', chs='\"') %>% as.numeric(),
  )



head(gc)
glimpse(gc)

gc %>% 
  write.csv(
    'E:/gc_lanjutan.csv',
    quote = F, row.names = F
  )

