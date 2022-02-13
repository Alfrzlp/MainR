# Menggunakan library arules
library(arules)

# Membaca transaksi dari file data_transaksi.txt
transaksi <- read.transactions(file = "https://academy.dqlab.id/dataset/data_transaksi.txt", format = "single", sep = "\t", cols = c(1, 2), skip = 1)

# Menampilkan data transaksi dengan print dan inspect
inspect(transaksi)

# Menghasilkan model Market Basket Analysis
mba <- apriori(transaksi, parameter = list(supp = 0.1, confidence = 0.5))

# Menampilkan paket produk
inspect(subset(mba, lift > 1))

# Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support = .1, minlen = 2, target = "frequent itemsets")))



# Membaca transaksi dari file data_transaksi2.txt
transaksi <- read.transactions(file = "https://academy.dqlab.id/dataset/data_transaksi2.txt", format = "single", sep = "\t", cols = c(1, 2), skip = 1)

# Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support = .03, minlen = 2, target = "frequent itemsets")))


transaksi@itemInfo
transaksi@itemsetInfo
# misal 1 gula ada di transaksi 10, 14
transaksi@data

itemFrequency(transaksi)
itemFrequency(transaksi, type = "absolute")





library(arules)
transaksi <- read.transactions(file = "https://academy.dqlab.id/dataset/data_transaksi.txt", format = "single", sep = "\t", cols = c(1, 2), skip = 1)

data_item <- itemFrequency(transaksi, type = "absolute")

# Melakukan sorting pada data_item
data_item <- sort(data_item, decreasing = TRUE)
print(data_item)
# Mengambil 3 item pertama
data_item <- data_item[1:3]
print(data_item)
# Konversi data_item menjadi data frame dengan kolom Nama_Produk dan Jumlah
data_item <- data.frame("Nama Produk" = names(data_item), "Jumlah" = data_item, row.names = NULL)

inspect(subset(mba, rhs %in% "Sirup"))
inspect(subset(mba, lhs %in% "Pet Food" & rhs %in% "Sirup"))

# lift lebih tinggi yang terbaik
# Tanpa parameter tambahan tersebut, maka nilai minimum support adalah 0.1
# dan minimum confidence adalah 0.8 sebagai filter dari function apriori.
mba <- apriori(transaksi, parameter = list(supp = 0.1, confidence = 0.5))

inspect(subset(mba, (lhs %in% "Shampo Biasa" | rhs %in% "Shampo Biasa") & lift > 1))

# Operator %in% yang sebelumnya kita gunakan sudah efektif.
# Namun operator ini tidak cocok jika kita ingin melakukan
# filter itemset dengan logika AND.
# Artinya seluruh item harus muncul dalam itemset yang terpilih.
# Untuk keperluan tersebut, gunakan operator %ain%.
inspect(subset(mba, (lhs %ain% c("Shampo Biasa", "Serum Vitamin"))))

library(arulesViz)
plot(subset(mba, lift > 1.1), method = "graph")
