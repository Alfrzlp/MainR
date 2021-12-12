library(gtrendsR)
library(tidyverse)

# 12 bulan lalu sampai sekarang
# remove low_search_volume
res = gtrends(c("data scientist", "business analyst"),
              gprop = c("web", "news", "images", "froogle", "youtube"),
              geo = "ID",
              low_search_volume = F,
              time = "today 12-m",
              onlyInterest = F,
              category = 0)

res$interest_over_time
res %>% 
  glimpse()

res$interest_over_time
res$interest_by_region
res$interest_by_city
res$related_queries

# lihat trend
res$interest_over_time %>% 
  ggplot() +
  geom_line(aes(x = date, y = hits, color = keyword), lwd = 1)+
  labs(x = "Tanggal", title = "Interest by Date")+
  scale_fill_viridis_c("Kata Kunci") +
  theme_minimal()


daerah = res$interest_by_region
daerah[is.na(daerah)]  <- 0

# bar chart tiap daerah
daerah %>% 
  ggplot() +
  geom_bar(aes(x = reorder(location, hits), weight = hits, fill = keyword)) +
  scale_fill_viridis_d(direction = -1)+
  labs(x = "Lokasi", y = "count")+
  coord_flip()+
  facet_wrap(~keyword)
  

gtrendsR::categories
# lihat 32 itu kategori apa
gtrendsR::categories %>% 
  filter(id == 32)
  
  
gtrendsR::countries %>% 
  filter(tolower(name) == "indonesia")
  
# lihat kode di indonesia
gtrendsR::countries %>% 
  filter(country_code == "ID")

  
mosaicplot(rep(c(2, 3), each = 3))
