library(tidyr)
library(dplyr)

table1
who
population

glimpse(who)
glimpse(population)

who %>%
  gather(key = "metadata", value = "cases", 5:60) %>%
  select(-metadata) %>%
  mutate(cases = replace_na(0)) %>%
  group_by(country, year) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  left_join(population) %>%
  filter(!is.na(population)) %>%
  library(stringr)
who %>%
  gather(key = "metadata", value = "cases", 5:60) %>%
  select(-iso2, -iso3) %>%
  distinct(metadata) %>%
  pull()

who %>%
  gather(key = "metadata", value = "cases", 5:60) %>%
  select(-iso2, -iso3) %>%
  # hapus new_ atau new
  mutate(metadata = str_remove(metadata, "(?:new_|new)")) %>%
  # memisahkan kolom metadata menjadi 2 berdasarkan _
  separate(metadata, c("method", "gender_age_group"), sep = "_") %>%
  # pisahkan gender_age_group menjadi 2 berdasarkan karakter 1
  separate(gender_age_group, c("gender", "age_group"), 1) %>%
  # ganti m menjadi male dan f menjadi female
  mutate(
    gender = recode(gender, m = "Male", f = "Female"),
    age_group = case_when(
      age_group == "65" ~ "65+",
      # digit ada 1 atau 2
      # digit 2
      TRUE ~ str_replace(age_group, "(\\d{1,2})(\\d{2})$", "\\1-\\2")
    )
  )



a <- c(18, 24, 16, 30, 32, 26, 20, 15, 10, -1)
b <- c(13, 25, 15, 32, 36, 24, 12, 16, -2, NA)
c <- runif(10, 10, 30)

# sum kolom jika bilangan kurang dari 0 dan ada NA maka NA
sum_pass <- function(x) {
  if (all(x >= 0)) {
    sum(x, na.rm = T)
  } else {
    NA_real_
  }
}

# banyak bilangan lebih dari 0 dan bukan NA
count_pass <- function(x) {
  sum(x >= 0 & !is.na(x), na.rm = T)
}

x <- c(1:10, NA, NA, -2)
sum(x[!is.na(x) & x >= 0])


data.frame(a, b, c) %>%
  rowwise() %>%
  mutate(
    n_pass = count_pass(c_across(cols = a:c)),
    sum = sum_pass((c_across(cols = a:c)))
  ) %>%
  ungroup()


# filter ------------------------------------------------------------------
df %>%
  dplyr::filter(
    !(absen %in% c(4, 5, 11, 33) & kelas == "3SK3")
  )
