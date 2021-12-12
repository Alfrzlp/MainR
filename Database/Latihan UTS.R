input_table <- function(str, nama, ...){
  #dbGetQuery(con, paste("DROP TABLE IF EXISTS", nama))
  df <- read.table(textConnection(str), ...) %>% 
    janitor::clean_names()
  dbWriteTable(con, nama, df)
}

str = 'idPelanggan namaDepan namaBelakang kota negara telepon
1 Maria Anders Berlin Germany 030-0074321
2 Ana Trujillo Mexico Mexico (5)555-4729
3 Antonio Moreno London UK (171)555-7788
4 Thomas Hardy Glasgow UK (171)555-2340
5 Andi Maulana Depok Indonesia +628568431209'

input_table(str, 'pelanggan', header = T)
dbListTables(con)

dbGetQuery(con, paste("DROP TABLE", 'Pelanggan'))
dbReadTable(con, "pelanggan")
