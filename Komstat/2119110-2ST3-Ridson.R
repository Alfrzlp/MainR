# Manipulasi dengan dplyr
library(dplyr)
library(nycflights13)
data("flights")

head(flights)
sample_n(flights, 10)

# select
flights[which(names(flights) == c("year", "month", "day", "arr_delay", "dep_delay"))]
# dengan dplyr
dplyr::select(flights, year, month, day, arr_delay, dep_delay)
# atau
dplyr::select(flights, year:day, arr_delay, dep_delay)

# drop Variable
newdata <- select(flights, -arr_delay, -c(year:day))
newdata <- select(flights, -c(arr_delay, year:day))

# select specific patern
# ambil yang ada patern arr
head(select(flights, -starts_with("arr")))
head(select(flights, -contains("time")))


# Filter
filter(flights, dep_delay > 1000)
filter(flights, origin == "JFK")
filter(flights, origin %in% c("JFK", "EWR"))
filter(flights, !origin %in% c("JFK", "EWR"))
filter(flights, origin %in% c("JFK", "EWR") & month > 5)
filter(flights, origin %in% c("JFK", "EWR") | carrier == "UA")
filter(flights, grepl("JB", tailnum))

# Arrange
arrange(flights, desc(dep_delay))

# summarise
# na remove artinya na tidak dihitung
summarise(flights, mean_dep_delay = mean(dep_delay, na.rm = T))

# mutate
new_flight <- mutate(flights, air_time_hours = air_time / 60)

# group by
summarise(group_by(flights, month), mean_dep_delay = mean(dep_delay, na.rm = T))

# piping
new_flight2 <- flights %>%
  filter(month == 5, day == 17, carrier %in% c("UA", "WN", "AA", "DL")) %>%
  select(carrier, dep_delay, air_time, distance) %>%
  arrange(carrier) %>%
  mutate(air_time_hours = air_time / 60)

head(new_flight2)


# join
df1 <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  w = c("a", "b", "c", "d", "e"),
  x = c(1, 1, 0, 0, 1),
  y = rnorm(5),
  z = letters[1:5]
)

df2 <- data.frame(
  ID = c(1, 7, 3, 6, 8),
  w = c("z", "b", "k", "d", "l"),
  x = c(1, 2, 3, 0, 4),
  y = rnorm(5),
  z = letters[2:6]
)

df1
df2

inner_join(df1, df2, by = "ID")
left_join(df1, df2, by = "ID")
right_join(df1, df2, by = "ID")
full_join(df1, df2, by = "ID")

anti_join(df1, df2, by = "ID")
