library(tidyverse)
library(tidytext)
library(lubridate)
library(wordcloud2)
library(tuber)

yt_oauth(app_id = "123117760113-61nkct1lijno2kf936l8ocpv0vs0s9j4.apps.googleusercontent.com",
         app_secret = "Sow5-XmyzBUUVDADZTcICoih", token = "")

#DPR Resmi Sahkan Omnibus Law RUU Ciptaker
komen = get_all_comments(video_id = "m-_EYWMK-Uk")
View(komen)
dim(komen)

komen = komen %>% select(textOriginal, authorDisplayName, likeCount, publishedAt, updatedAt)
head(komen)

#Perbaiki tanggal
komen = komen %>% mutate(publishedAt = ymd_hms(publishedAt),
                 updatedAt = ymd_hms(updatedAt)) 
glimpse(komen)

#frekuensi komentar
rtweet::ts_plot(komen, by="hours")

#kata yang sering diucapkan
komen %>% select(textOriginal) %>% 
  unnest_tokens(kata, textOriginal) %>% 
  group_by(kata) %>% count(sort = T) %>% 
  anti_join(stopword_indo, by=c("kata" = "V1")) %>% 
  wordcloud2()


#=================================================================

#ambil semua video
maq = get_all_channel_video_stats(channel_id = "UClEZXxVmQTI5o-ZFT_HDXvA")
dim(maq)
view(maq)

#data cleaning
glimpse(maq)
#lihat jumlah na
maq %>% summarise_all( funs(sum(is.na(.)) ))

maq = maq %>% 
  select(-url, -channel_id, -channel_title) %>% 
  type.convert() %>% 
  mutate(publication_date = ymd_hms(publication_date)) %>% 
  arrange(desc(publication_date)) %>% 
  rowid_to_column()


#visulisai semua video
ggplot(maq) +
  geom_line(aes(x=publication_date, y=likeCount), col="green") +
  geom_line(aes(x=publication_date, y=dislikeCount), col="red") +
  geom_line(aes(x=publication_date, y=commentCount), col="black") +
  theme_minimal() +
  labs(x = "Date", y="count")


#filter video di tahun 2020
maq %>% 
  filter(publication_date >= "2020-01-01 00:00:00") %>% 
  ggplot() +
  geom_line(aes(x=publication_date, y=likeCount), col="green", size=1) +
  geom_line(aes(x=publication_date, y=dislikeCount), col="red", size=1) +
  geom_line(aes(x=publication_date, y=commentCount), col="yellow", size=1) +
  theme_minimal() +
  labs(x = "Date", y="count") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "cyan", size = 15),
        title = element_text(colour = "gold"),
        panel.background = element_rect(colour = "black", fill = "black"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "white", size = 11),
        legend.key = element_blank(),
        legend.key.size = unit(3, "lines"),
        legend.key.height = unit(0, "cm"))


#video dengan penonton terbanyak
maq[which.max(maq$viewCount),]

#video dislike terbanyak
maq[which.max(maq$dislikeCount),]

summary(maq)

