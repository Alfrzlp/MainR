library(rtweet)
## install dev version of rtweet from github
# remotes::install_github("ropensci/rtweet")


trends_available()

create_token(app = "App_text mining",
             consumer_key = "Lbl3URg7B7MuVWe8oEigDDDR0",
             consumer_secret = "qVmj3c2IU6hU4BK92IAYI3aXtYHzHcypSN0ejV9pIsyIMQrLay",
             access_token = "1282149965985280000-nKUchRGUKIpKWPIpmeEWOY6nJ2qCqh",
             access_secret = "ZCJJ68egtAWyiNJsV6XVBcV0ULgzY4sj4Mt3DfjZWox88",
             set_renv = T)

token = create_token(access_token = token, access_secret = secrettoken,
             consumer_key = apikey, consumer_secret = apisecretkey)


indo = get_trends("indonesia")
View(indo)

#Ambil data twiter berdasarkan kata kunci
tweet <- search_tweets(
  q = "Vaksin",
  n = 1000,
  include_rts = T,
  type = "recent",
  lang = "id" #bahasa
)

tweet
View(tweet)

#secs, mins, hours, days, weeks, months, or years
ts_plot(tweet , by="days") + 
  theme_minimal() +
  labs(title = "Frekuesnsi Tweet dengan kata Kunci #Omnibuslaw",
       subtitle = "Per hari", y="Frekuensi", x="waktu dalam hari")



library(dplyr)
tweet %>% select(text) %>% 
  mutate(text = gsub("[[:punct:]]", "", text)) %>% 
  #bersihkan emoticon dan lain-lain dan simpan di text.cleaned
  plain_tweets() -> text.cleaned

library(tidytext)
text.cleaned %>% 
  #tokenize semua kalimat menjadi perkata
  unnest_tokens(input = text, output = token) %>% 
  count(token, sort = T)

stopword_indo <- read.csv("https://raw.githubusercontent.com/masdevid/ID-Stopwords/197fcfccbe4b2b873fb090a6e46f1bc1a530e457/id.stopwords.02.01.2016.txt",
                          header = F)
head(stopword_indo)

library(wordcloud2)
text.cleaned %>% 
  unnest_tokens(input = text, output = token) %>% 
  anti_join(stopword_indo, by=c("token" = "V1")) %>% 
  count(token, sort = T) %>% 
  wordcloud2(size=1)


#atau versi saya
text.cleaned %>% 
  unnest_tokens(input = text, output = token) %>% 
  filter(!token %in% stopword_indo$V1) %>% 
  count(token, sort = T) %>% 
  wordcloud2(size=1)

#==========================================================================
stream_tweets(
  q = "#Omnibuslaw",
  timeout = 10, #10 detik
  file_name = "streamtwet.json",
  #jangan diparsing agar menghemat rsource
  parse = F
)
parse_stream("streamtwet.json")

#=========================================================================
#mengambik time line dari suatu akun
timeline.jokowi <- get_timeline(user = "jokowi", n=1000)
View(timeline.jokowi)

#melihat kata yang sering dikatakan
library(stringr)
timeline.jokowi %>%  select(text) %>% 
  mutate(text = str_remove_all(text, "https"),
         text = str_remove_all(text, "t.co"),
         text = removePunctuation(text)) %>% 
  unnest_tokens(input = text, output = token) %>% 
  anti_join(stopword_indo, by=c("token" = "V1")) %>% 
  group_by(token) %>% 
  count(sort = T) %>% 
  wordcloud2(minRotation = 0, maxRotation = 0)

timeline.prabowo <- get_timeline(user = "prabowo", n=1000)
View(timeline.prabowo)

#Frekuensi Tweet bapak Jokowi per minggu
timeline.jokowi %>% 
  ts_plot("weeks")

#per bulan
timeline.jokowi %>% 
  ts_plot("months")
timeline.prabowo %>% 
  ts_plot("months")


#Mencari berapa banyak follower
bio.jokowi <- lookup_users(users = "jokowi")
bio.prabowo <- lookup_users(users = "prabowo")

View(bio.jokowi)
View(bio.prabowo)

follower <- c(bio.jokowi$followers_count, bio.prabowo$followers_count)
follower = data.frame(nama = c("Jokowi", "Prabowo"), jumlah_follower = follower)
follower

library(ggplot2)
ggplot(follower)+
  geom_col(aes(x=nama, y=jumlah_follower, fill=nama)) +
  labs(title = "Jumlah Followers")
