x <- readLines('D:/Downloads/DatabaseMunawir.txt', encoding = 'UTF-8')
x
length(x)

db <- read.table('D:/Downloads/DatabaseMunawir.txt', encoding = 'UTF-8',
                 sep = '\t', quote = "")
db <- `colnames<-`(db, c('indo', 'arab'))

Encoding(db$arab)
view(db)
dim(db)
head(db)


db %>% dplyr::filter(str_detect(indo, 'memukul'))
db %>% dplyr::filter(grepl('??????', arab, perl = T))


library(RPostgreSQL)
library(DBI)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'kamus', user = "ridsonap",
  password = "ridson"
)

# melihat tabel apa saj yang ada
dbListTables(con)

dbWriteTable(con, "data", db, overwrite = T)



