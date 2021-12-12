library(tidyverse)
library(treemapify)
library(treemap)

treemapify(G20, area = "gdp_mil_usd")
ggplot(G20, aes(area = gdp_mil_usd, fill = region)) +
  geom_treemap()

G20

df <- readxl::read_xls("C:/Users/Ridson Alfarizal/Downloads/Indo_29_4976153.xls") %>% 
  drop_na(1) %>% 
  mutate_all(function(x){ ifelse(x == '-', 0 ,x) }) %>% 
  type_convert()

df %>% 
  filter(`Kabupaten/Kota` %in% c('Pasuruan', 'Sidoarjo', 'Kediri', 'Jombang')) %>% 
  select(-Jumlah) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(area = value, fill = name, label = paste(round(value, 2), '%'),
             subgroup = `Kabupaten/Kota`, subgroup2 = name)) +
  geom_treemap() +
  geom_treemap_subgroup_border()+
  geom_treemap_subgroup2_border(colour = 'black')+
  # geom_treemap_subgroup_text(place = 'centre', alpha = 0.5)+
  geom_treemap_text(fontface = 'italic', colour = 'white',
                    place = 'topleft', grow = F, reflow = T, min.size = 0)+
  labs(title = 'Sumber Air Minum', fill = '')+
  #theme(legend.position = 'none')+
  facet_wrap(~`Kabupaten/Kota`)

