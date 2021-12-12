library(rtweet)
library(textclean)
library(katadasaR)

tweet = parse_stream("streamtwet.json")
view(tweet)
sample_n(df, 10)

read.csv("D:/Datasets/NLP/Lazada/20191002-reviews.csv") %>% 
  select(reviewContent) %>% 
  filter(reviewContent != "null") %>% dim()



tweet %>% 
  select(text) %>% 
  mutate(text = gsub("\\n", "\\s", text)) %>% 
  
  replace_html() %>% 
  replace_url() %>% 
  
  replace_emoji() %>% 
  replace_html() %>% 
  
  replace_tag(text, pattern = "@([A-Za-z0-9_]+)") %>% 
  replace_hash(text, pattern = "@([A-Za-z0-9_]+)") %>% 
  
  strip()# hapus titik spasi dan lain2 yang tidak penting
  
katadasaR("mengetahui")
