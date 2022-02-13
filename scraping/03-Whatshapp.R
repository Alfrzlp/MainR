library(tidyverse)
library(tidytext)
library(rwhatsapp)
library(ggplot2)

history <- rwa_read("D:/Datasets/Chat WhatsApp dengan KaeSeM.txt")
head(history)

# Data cleaning
# hapus kolom id dan source
history <- history %>% select(!c(source, id))

# lihat NA pada author
history %>% summarise(na_author = sum(is.na(author)))

# lihat dimana baris NA author
history %>% filter(is.na(author))

# Hapus Na jika author na
history <- history %>% tidyr::drop_na(author)
dim(history)

# Siapa yang paling banyak chat
history %>%
  group_by(author) %>%
  count() %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(author, n), fill = reorder(author, n)), alpha = 0.9) +
  geom_text(aes(x = n, y = author, label = n), hjust = 1.5) +
  theme_minimal() +
  scale_fill_viridis_d("Nama") +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) +
  ggtitle("Banyaknya Pesan dikirim")


# Hari apa paling rame
# jadikan faktor
history <- history %>% mutate(day = weekdays(as.Date(time)))
head(history)

hari <- c("Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu", "Minggu")
history$day <- factor(history$day, hari)

history %>%
  group_by(day) %>%
  count() %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(day, 7:1), fill = day)) + # dibalik dari senin
  geom_text(aes(x = n, y = day, label = n), hjust = 3) +
  scale_fill_viridis_d("Hari") +
  theme_minimal() +
  labs(y = "Hari")


# memisahkan
history %>%
  separate(time, c("tahun", "bulan", "tanggal", "jam", "menit", "detik"))


# stopword
stopwords <- read.csv("D:/Datasets/stopwordbahasa.csv")
head(stopwords)

# Kata yang sering diucapkan
history %>%
  filter(text != "<Media tidak disertakan>") %>%
  unnest_tokens(kata, text) %>%
  anti_join(stopwords, by = c("kata" = "stopword")) %>%
  group_by(kata) %>%
  count(sort = T) %>%
  wordcloud2::wordcloud2()

# kalender
library(lubridate)
history %>%
  separate(time, c("tanggal", "jam"), sep = "\\s") %>%
  mutate(tanggal = ymd(tanggal)) %>%
  group_by(tanggal) %>%
  count() %>%
  ungroup() %>%
  mutate(tanggal = as.Date(tanggal)) %>%
  ggplot_calendar_heatmap(
    "tanggal",
    "n"
  ) +
  scale_fill_continuous(low = "green", high = "red")
