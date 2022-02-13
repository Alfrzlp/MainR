library(tidyverse)

# Female Births -----------------------------------------------------------

df <- read.csv("D:/Datasets/__Time Series/daily-total-female-births.csv") %>%
  type_convert() %>%
  janitor::clean_names()

df %>% head()

df %>%
  ggplot(aes(date, births)) +
  geom_line(colour = "steelblue", lwd = 1, ) +
  labs(
    x = NULL,
    y = "Births",
    title = "Daily Total Female Births",
    subtitle = "California, 1959"
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(
    expand = expansion(mult = c(-0.3, 0))
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  theme_classic(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray30"),
    plot.title = element_text(
      family = "Arial", face = "bold",
      colour = "black", size = rel(1.75)
    ),
    plot.subtitle = element_text(
      family = "Arial",
      colour = "black", size = rel(1.25)
    ),
    axis.title.y = element_text(hjust = 1),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = "gray70"),
    plot.margin = margin(15, 15, 15, 15),
  )






# Temperatur --------------------------------------------------------------

df <- read.csv("D:/Datasets/__Time Series/daily-min-temperatures.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  janitor::clean_names()

df %>% head()

df %>%
  filter(date > "1989-01-01") %>%
  ggplot(aes(date, temp)) +
  geom_line(colour = "steelblue", lwd = 1) +
  labs(
    x = NULL,
    y = "Temperature",
    title = "Minimum Daily Temperatures",
    subtitle = "Melbourne, Australia"
  ) +
  expand_limits(y = 0) +
  scale_x_date(
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  theme_classic(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray30"),
    plot.title = element_text(
      family = "Arial", face = "bold",
      colour = "black", size = rel(1.75)
    ),
    plot.subtitle = element_text(
      family = "Arial",
      colour = "black", size = rel(1.25)
    ),
    axis.title.y = element_text(hjust = 1),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = "gray70"),
    plot.margin = margin(15, 15, 15, 15),
  )



# Dekomposisi -------------------------------------------------------------
head(df)

df <- df %>%
  filter(date > "1989-01-01")

mdt <- ts(df$temp, frequency = 12)
mdt
mdt.dec <- decompose(mdt, type = "mul")

decompose(mdt, type = "mul") %>%
  autoplot()


library(forecast)
seasonplot(mdt)
naive(seasadj(mdt.dec))
