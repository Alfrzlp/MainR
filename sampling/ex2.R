library(foreign)

loc <- "C:/Users/Ridson Alfarizal/Documents/Main SPSS/kor07rmt__.sav"
# jika use.value.labels False maka yang ada di data frame 0,1
# jika True maka akan spt "layak", "tidak layak"
df <- read.spss(loc, to.data.frame = T, use.value.labels = T)

df <- df[, -144]
View(df)

# membuat vektor isi NA
status_orang <- rep(NA, dim(df)[1])

for (i in 1:dim(df)[1]) {
  if (df[, "RmhKecil"][i] == "Rumah Besar" & df[, "StatRmh"][i] == "Milik Sendiri") {
    status_orang[i] <- 1
  } else {
    status_orang[i] <- 0
  }
}
df <- cbind(df, status_orang)

dim(df)
sum(df[, "status_orang"])

# menghapus 14 kolom terakhir
df <- df[, 1:(length(df) - 14)]

quantile(df[, "exp_cap"])
fivenum(df[, "exp_cap"])
# bentuk umumnya
fivenum(df$exp_cap)

# dgn range
pp_baru <- c()
for (i in df$b7r25) {
  if (i < 250000) {
    pp_baru <- append(pp_baru, 1)
  } else {
    pp_baru <- append(pp_baru, 0)
  }
}
sum(pp_baru)
#------------------------------------------------------------------------------------
# recode
atap_baru <- c()
for (i in df$b6r2) {
  if (i == "Genteng" | i == "Seng" | i == "Beton") {
    atap_baru <- append(atap_baru, 1)
  } else {
    atap_baru <- append(atap_baru, 0)
  }
}
atap_baru
#------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gapminder)

gapminder
loc <- "D:/R/gapminder.tsv"
data <- read.csv(loc, header = TRUE, sep = "\t")
View(gapminder)

databaru <- data %>% filter(country == "Congo, Dem. Rep.")

data %>%
  arrange(desc(gdpPercap)) %>%
  filter(year == 2007)

databaru
ggplot(databaru, aes(year, gdpPercap)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Gdp Percap", title = "Congo, Dem, Rep")

#------------------------------------------------------------------------------
library(ggplot2)
data <- data.frame(
  murder = USArrests$Murder,
  state = tolower(rownames(USArrests))
)
View(data)
map <- map_data("state")
View(map)
k <- ggplot(data, aes(fill = murder))
k + geom_map(aes(map_id = state), map = map) + expand_limits(x = map$long, y = map$lat)
