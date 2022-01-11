library(RPostgreSQL)
library(DBI)

# nama database
db = 'kamus'

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'kamus', user = "ridsonap", password = "ridson")

# melihat tabel apa saj yang ada
dbListTables(con)


# membuat tabel
df = mtcars %>% 
  mutate(carname = rownames(mtcars), .before = mpg)
df
dbWriteTable(con, "data", db, overwrite = T)

bandara[is.na(bandara)] = NULL

# baca tabel
dbReadTable(con, "tabelmahasiswa")
dbReadTable(con, "cars")
dbGetQuery(con, "SELECT * FROM cars")



# jadikan carname primary key
dbGetQuery(con,'ALTER TABLE cars ADD CONSTRAINT cars_pk PRIMARY KEY ("carname")')

# contoh perintah
dbGetQuery(con, 'select * from cars order by mpg DESC')


# kalau pakai dbSendQuery harus diakhiri dbClearResult 
query = dbSendQuery(con, "SELECT * FROM cars")
result = dbFetch(query)
result
dbClearResult(query)


# edit tabel
dbGetQuery(con, "SELECT * FROM cars")

date = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 12)


x = sample(1:1000, 12)
x = paste0( "(", as.character(x) %>% 
  paste(collapse = "), ("), ")")
x


# hapus tabel
dbRemoveTable(con, "mtcars")

# akhir
dbDisconnect(con)
