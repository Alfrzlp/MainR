kebun_jatim <- read.csv("D:/Datasets/kebun_jatim.csv", sep = ";")
head(kebun_jatim)

library(janitor)
kebun_jatim <- clean_names(kebun_jatim)
head(kebun_jatim)
