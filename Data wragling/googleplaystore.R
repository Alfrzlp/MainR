library(tidyverse)
library(highcharter)
library(lubridate)
library(stringr)
library(xts)

data <- read.csv("D:/Datasets/googleplaystore.csv")

data.clean <- data %>%
  mutate(
    # Eliminate some characters to transform Installs to numeric
    Installs = gsub("\\+", "", as.character(Installs)),
    Installs = as.numeric(gsub(",", "", Installs)),
    # Eliminate M to transform Size to numeric
    Size = gsub("M", "", Size),
    # Replace cells with k to 0 since it is < 1MB
    Size = ifelse(grepl("k", Size), 0, as.numeric(Size)),
    # Transform reviews to numeric
    Reviews = as.numeric(Reviews),
    # Remove currency symbol from Price, change it to numeric
    Price = as.numeric(gsub("\\$", "", as.character(Price))),
    # Last Updated to date format
    Last.Updated = mdy(Last.Updated),
    # Replace "Varies with device" to NA since it is unknown
    Min.Android.Ver = gsub("Varies with device", NA, Android.Ver),
    # Keep only version number to 1 decimal
    Min.Android.Ver = as.numeric(substr(Min.Android.Ver, 1, 3)),
    # Drop old Android version column
    Android.Ver = NULL
  ) %>%
  filter(
    # Two apps had type as 0 or NA, they will be removed
    Type %in% c("Free", "Paid")
  )

View(data.clean)
dim(data.clean)

data.clean <- data.clean %>% distinct(App, .keep_all = TRUE)

# melihat jumlah na tiap kolom
data.clean %>% summarise_all(funs(sum(is.na(.))))

library(ggplot2)
library(dplyr)
library(stringr)

a <- data.clean %>%
  group_by(Category) %>%
  summarise(total_install = sum(Installs)) %>%
  arrange(total_install) %>%
  ggplot() +
  geom_col(aes(
    x = total_install,
    y = reorder(str_to_title(Category), total_install),
    fill = reorder(Category, total_install)
  )) +
  scale_fill_viridis_d(direction = -1) +
  theme(legend.position = "none") +
  labs(
    title = "Most Popular category (by Total Installs)",
    x = "Total Installs", y = "Category"
  )

b <- data.clean %>%
  group_by(Category) %>%
  count(Type) %>%
  filter(Type == "Paid") %>%
  arrange(n) %>%
  ggplot() +
  geom_col(aes(
    x = n,
    y = reorder(str_to_title(Category), n),
    fill = reorder(Category, n)
  )) +
  geom_text(aes(
    x = n + 3, y = reorder(str_to_title(Category), n),
    label = n
  )) +
  scale_fill_viridis_d(direction = -1) +
  theme(legend.position = "none") +
  labs(
    title = "Kategori paling banyak Aplikasi Berbayar",
    y = "kategori", x = "banyaknya aplikasi"
  )

b
library(gridExtra)
grid.arrange(a, b, ncol = 2, nrow = 1)

library(ggpubr)
ggarrange(a, b, labels = c("a", "b"), ncol = 2, nrow = 1)

data.clean[which.max(data.clean$Reviews), ]


str <- "10,12,1,3,1,4,21,3"
a <- as.numeric(as.vector(strsplit(str, ",")[[1]]))
a
sum(a)
a[1]
