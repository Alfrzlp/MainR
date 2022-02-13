# id SalePrice
library(caret)
library(tidyverse)

train <- read.csv("D:/_Datasets/House Prices/train.csv") %>%
  janitor::clean_names()
test <- read.csv("D:/_Datasets/House Prices/test.csv") %>%
  janitor::clean_names()

glimpse(train)
