library(dplyr)

loc <- "D:/__Semester7/datmin/data"

dat <- read.csv(file.path(loc, "data_preprocessing.csv"))
head(dat)
tail(dat)

# Melihat jumlah NA
colSums(is.na(dat))


# Imputasi NA dan ubah tipe data menjadi factor
dat <- dat %>% 
  mutate(
    age = ifelse(is.na(age), mean(age, na.rm = T), age),
    salary = ifelse(is.na(salary), mean(salary, na.rm = T), salary),
    purchased_item = factor(purchased_item, levels = c('No', 'Yes'), labels = c(0,1)),
    nation = factor(nation, levels = c('India', 'Germany', 'Russia'), labels=c(1,2,3))
  )

glimpse(dat)
  


bank <- read.csv(file.path(loc, "bank.csv"), sep = ";")
glimpse(bank)

# Melihat jumlah NA
colSums(is.na(bank))

# Tidak ada Na tidak perlu imputasi