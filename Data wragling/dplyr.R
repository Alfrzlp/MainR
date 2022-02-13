library(dplyr)
library(ggplot2)
library(gapminder)
library(foreign)

loc <- "C:/Users/Ridson Alfarizal/Documents/Main SPSS/kor07rmt__.sav"
# jika use.value.labels = False maka yang ada di data frame 0,1
# jika True maka akan spt "layak", "tidak layak"
df <- read.spss(loc, to.data.frame = T, use.value.labels = T)

gapminder
df

gapminder %>% select(country, pop) # hanya menampilkan country dan pop
#------------------------------------------------------------------------
# filter = mengelompokkan baris sesuai filternya
databaru <- gapminder %>% filter(country == c("Indonesia", "Malaysia"))
databaru

ggplot(databaru, aes(x = year)) +
  geom_line(aes(y = gdpPercap, color = country, group = country))
ggplot(databaru, aes(x = year)) +
  geom_line(aes(y = lifeExp, color = country, group = country))

# pengeluaran perkapita < 250.000
nrow(df %>% filter(b7r25 < 250000))

# pengeluaran perkapita < 250.000 tetapi rumah = layak
nrow(df %>% filter(b7r25 < 250000, Layak == "Layak"))

#------------------------------------------------------------------------
# arrange = menyusun
gapminder %>% arrange(continent) # menyusun urut continent
gapminder %>%
  arrange(desc(gdpPercap)) %>%
  filter(year == 2007) # urut dgp

#------------------------------------------------------------------------
# count = menhitung banyaknya rumah tidak layak misalnya

df %>% count(Layak, sort = T)
# status rumah dan rumah tidak layak
df %>% count(StatRmh, Layak, sort = T)
# bisa dengan bobot wt = colom di data set

gapminder$baru <- cut(gapminder$gdpPercap,
  breaks = c(-Inf, 400, 800, Inf),
  labels = c(0, 1, 2)
)

#------------------------------------------------------------------------
# mutate = bisa buat variabel baru/ recode variabel lama
gapminder %>% mutate(pendapatan_perkapita = gdpPercap / pop)

#-----------------------------------------------------------------------
# rename = ganti nama kolom
df <- rename(df, Kelayakan_Rumah = Layak)
df <- df[-144]
# b7r25

#------------------------------------------------------------------------
# group_by, summarise
gapminder %>%
  group_by(country) %>%
  filter(continent == "Asia") %>%
  summarise(avg_gdp = mean(gdpPercap), avg_lifeExp = mean(lifeExp))


#-------------------------------------------------------------------------
df <- df %>% mutate(a = 1:nrow(df))
# sama dengan
library(magrittr)
df %<>% mutate(a = 1:nrow(df))

# ingin ubah data tapi ingin plot juga
attach(df)
df <- df %>% mutate(a = 1:nrow(df)) %T>% plot(jumlah)

#-------------------------------------------------------------------------
# remove rows if contain na
# 1. nomor data tidak dirubah
na.omit(airquality)

# 2. nomor data diubah diurutkan lagi
library(tidyr)
airquality %>% drop_na()

# jika ingin beberapa kolom tertentu
airquality %>% drop_na(Wind, Solar.R)

library(dplyr)
airquality %>%
  drop_na() %>%
  group_by(Month) %>%
  summarise_all(mean)

#---------------------------------------------------------------------------
df %>% mutate_at(c("FID", "KODE", "Kasus_pos", "Kasus_sem", "Kasus_meni"), as.numeric)
df %>% mutate_at(c(1:2, 4:6), as.numeric)
df %>% mutate_at(c(-3), as.numeric)
df %>% mutate_if(is.list, as.numeric)


st <- data.frame(state.x77)
st %>% summarise_all(.funs = function(x) sd(x) / mean(x))


x <- c(45, 55, 70)
y <- c(905, 890, 870)

data <- t(data.frame(x, y, jumlah = x + y)) %>%
  data.frame() %>%
  mutate(total = rowSums(.[1:2])) # rowsum
# =======================================================================
pakan1 <- c(60.8, 57, 65, 58.6, 61.7)
pakan2 <- c(68.7, 67.7, 74, 66.3, 69.8)
pakan3 <- c(102.6, 102.1, 100.2, 96.5, 103.5)
pakan4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

data <- data.frame(pakan1, pakan2, pakan3, pakan4)

# make |Zij-zmean i|
data %>% mutate_all(~ abs(.x - mean(.x)))



data %>%
  group_by(jenis_kelamin) %>%
  summarise(p = n() / nrow(data))


dat %>%
  mutate_all(~ str_remove_all(.x, "\\s")) %>% # Hapus spasi
  mutate_at(2:11, ~ str_replace_all(.x, ",", ".")) %>%
  type_convert() %>%
  as.data.frame()

# sample by group ---------------------------------------------------------

dat4 <- dat %>%
  select(diabetes, glucose) %>%
  group_by(diabetes) %>%
  nest() %>%
  ungroup() %>%
  mutate(n = c(30, 497)) %>%
  mutate(samp = map2(data, n, sample_n)) %>%
  select(-data) %>%
  unnest(samp) %>%
  select(-n)
