mpg <- ggplot2::mpg
mpg %>% 
  distinct(manufacturer)

mpg %>% 
  split(.$manufacturer) %>% 
  {.$honda}

mpg %>% 
  group_by(manufacturer) %>% 
  # x name (yang uniq) & value : value
  group_walk(~ assign(x = .y$manufacturer, value = .x, 
                      envir = .GlobalEnv))

library(readxl)
library(purr)

dokumen <- fs::dir_ls("D:/Datasets/Harga") %>% 
  fs::dir_ls()

dokumen <- fs::dir_ls("D:/Datasets/Harga/Harga Mobil",
           #glob = "*.csv",
           regexp = "(train|validate)_\\w+[.]csv$") 

map_dfr(dokumen, read.csv)



# banyak
map_dfr(dokumen, ~map_df(excel_sheets(.x), read_excel, path = .x))