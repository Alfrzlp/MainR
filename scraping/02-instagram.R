# cek
# where instagram-scraper

# m = maksimum post
# 20 post terakhir
# media-types none -- tidak mengambil media
# ambil comments
system("instagram-scraper gnfi -m 20 --media-types none --comments")

fs::dir_tree("C:/Users/Ridson Alfarizal/eb_adst")
system("eb_adst/eb_adst.json")

library(jsonlite)
library(tidyverse)
library(listviewer)
library(wordcloud2)

desi <- fromJSON("C:/Users/Ridson Alfarizal/eb_adst/eb_adst.json")
desi
str(desi)

desi %>%
  pluck(1) %>% # menaikan objek pertama
  jsonlite::flatten() %>% # menaikan semua objek yang tidak penting
  jsonedit() # melihat objek

desi %>%
  pluck(1) %>% # menaikan objek pertama
  jsonlite::flatten() %>% # menaikan semua objek yang tidak penting
  as_tibble() %>%
  view()

# Menghapus
gsub("https\\S*", "", str)
gsub("@\\S*", "", str)


desi %>%
  pluck(1) %>% # menaikan objek pertama
  jsonlite::flatten() %>% # menaikan semua objek yang tidak penting
  as_tibble() %>%
  janitor::clean_names() %>%
  rowid_to_column() %>% # memberi id
  select(
    postid = rowid, tags,
    time = taken_at_timestamp,
    n_like = edge_media_preview_like_count,
    comment = comments_data
  ) %>%
  mutate(
    tags = map_chr(tags, toString),
    time = as.POSIXct(time, origin = "1970-01-01")
  ) %>% # ekstrak coment
  unnest_wider(comment) %>%
  unnest() -> desi2


desi2 %>%
  group_by(username) %>%
  count() %>%
  ggplot2::ggplot() +
  geom_col(aes(x = reorder(username, n), y = n)) +
  coord_flip()

desi2 %>%
  select(text) %>%
  mutate(text = gsub("@\\S*", "", text)) %>%
  rtweet::plain_tweets() %>%
  tidytext::unnest_tokens(input = text, output = token) %>%
  group_by(token) %>%
  anti_join(stopwords, by = c("token" = "stopword")) %>%
  count(sort = T) %>%
  wordcloud2()



data <- fromJSON("Main Python New/viz/flow.json")
data %>%
  jsonedit()



data <- data$RWS$RW[[1]]

str(data)




qt(0.975, 18)
