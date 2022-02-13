library(tidyverse)

df.items <- read.csv("D:/Datasets/Lazada/20191002-items.csv")
df.reviews <- read.csv("D:/Datasets/Lazada/20191002-reviews.csv")

View(df.items)
View(df_reviews)

# Analisis jumlah NA
df.items %>% summarise_all(.funs = function(a) sum(is.na(.)))
df.reviews %>% summarise_all(.funs = function(a) sum(is.na(.)))

# analisis deskriptif
summary(df.items)
summary(df.reviews)

library(pastecs)
stat.desc(df.items) # terstruktur
stat.desc(df.reviews)
