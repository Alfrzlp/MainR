data = "1 8,5 6,6
2 9,1 7,9
3 9,6 6,9
4 9,3 7,6
5 7,9 7,1
6 7,8 8,7
7 6,5 7,3
8 6,8 6,7
9 7,0 6,2
10 7,2 6,8"

str.to.df <- function(str, pemisah=" ", dim, koma=T){
  library(stringr)
  library(dplyr)
  
  if(koma) str = str_replace_all(str, ",", ".")
  
  str = str_replace_all(str, "\n", ",")
  str = str_replace_all(str, pemisah, ",")
  str = str_split(str, ",", simplify = T)
  str = data.frame(matrix(str, nrow = dim[1], ncol=dim[2], byrow = T))
  
  for(i in 1:ncol(str)){
    tryCatch({str[,i] = as.numeric(str[,i])}, 
             warning = function(a) str[,i] = str[,i])
  }
  return(str)
}





str2vec <- function(str, adakoma = T, pemisah = "\\s"){
  str = gsub(",", ".", str)
  str = gsub(paste0(pemisah, "|\t|\n"), " ", str)
  str = str_split(str, pattern = " ")[[1]]
  # ambil yang ada angka dan ubah jadi numeric
  vec = as.numeric(str[str_detect(str, "[:alnum:]")])
  return(vec)
}





str = "aku 1927 awakmu? ddd dd ddd"

str_remove_all(str, "\\d")
str_remove(str, "\\d{4}")
#hapus digit dengan jumlah antara 1 2
str_remove(str, "\\d{1,2}")
#lebih dari 1
str_remove(str, "\\d{1,}")
str_remove(str, "\\d{5,}")

#hapus selain angka
str_remove_all(str, "\\D")


# hapus spasi awal akhir
df = df %>% 
  mutate(nama = stringi::stri_trim_both(nama))

# deteksi akhir
str_detect("daaad", "d$")
# awal
str_detect("daaad", "^d")


str_detect("ira rap", "^ir\\s")


# s boleh ada boleh nggak
# ? 0 atau 1,
# + 1 atau lebih,
# * 0 atau lebih
# {n} pas n banykanya
# {n, } pas n atau lebih
# {n, m} pas antara n m
# ?? 0 atau 1 prefer 0
# 
string <- "http:"
str_detect(string, "https?:")


"[^abc]"
# semuanya kecuali abc

# hapus spasi awal dan akhir
trimws(x)





# extrak ------------------------------------------------------------------
library(tidyverse)

data.frame(nama = c("D:apalah ini kalau/bahan-kue/puding/Aceh.csv")) %>% 
  extract(nama, into = c("a", "b", "c"),
          regex = ".*/(.*)/(.*)/(.*)\\.csv$",
          remove = F)


# Text --------------------------------------------------------------------

s <- "1. Diva - Malang - Man 2 kota mlg
2. Lala - Trenggalek - SMAN 1 Trenggalek
3. Nada - Trenggalek - SMAN 1 Trenggalek
4. Evita - Trenggalek - SMAN 1 Trenggalek
5. Luthfi - Tulungagung - SMAN 1 Trenggalek
6. Haqqul - Malang - SMAN Taruna Nala Jawa Timur
7. Hesti - Nganjuk - SMAN 2 Nganjuk
8. Tegar - Nganjuk - SMAN 2 Nganjuk
9. Shiva - sidoarjo - man 2 kota malang 
10. Akhadiyah - Nganjuk - SMAN 2 Kediri
11. Thessa - Kediri - SMAN 2 Kediri
12. Naufal - Nganjuk - SMAN 2 NGANJUK
13. Rey - Surabaya - SMAN 1 Cerme
14. Dita - Kediri - SMAN 2 Kediri
15. Niken -Magetan - SMAN 1 MAGETAN
16. Zhelvina Halim-Mojokerto- SMAN 1 Gondang
17. Fabian La Wima Vallessy - Lamongan - SMAN 2 Lamongan
18. Achmad Arfiandis Abdi Pradana - Lamongan - SMAN 2 Lamongan
19. Aul - Kediri - MAN 2 Kota Kediri
20. Arswenda Putra Maulana - Ponorogo - SMAN 1 Ponorogo
21. Sabrina - Tuban - SMA N 1 TUBAN
22. Regita Dewanti Rudianto - Nganjuk - SMAN 2 Nganjuk
23. Linda - Blitar - SMAN 3 Blitar
24. Srikandih Velayati - Jombang - SMAN 2 Jombang
25. Sinta - Blitar - SMAN 1 Blitar
26. Martha Budi Wardani - Jombang - SMAN 3 Jombang
27. Imelda Salsabila -Tulungagung-SMAN 1 Kedungwaru
28. Eka Putri L.C-Banyuwangi-Sman 1 Genteng
29. Aisha Arthamevia - Sidoarjo - SMAN 1 SIDOARJO
30.Sukma Andini- Banyuwangi - SMAN 1 genteng
31. Kautsar Hilmi I-Malang- SMAN 3 Malang
32. M Naufal Faishal - Malang - SMAN 3 Malang
33. Yusrina - Madiun - SMAN 2 Madiun
34. Abi - Jember - SMAN 1 Jember
35. Yhassesa - Madiun - SMAN 2 Madiun
36. Luthfio - Madiun - SMAN 2 Madiun
37. Alma Rohmah Fusur - Lamongan - MAN 2 LAMONGAN
38. Alya Rudita - Tulungagung -SMAN 1 Boyolangu
39. M. Farhan - Bondowoso - SMAN 2 Bondowoso
40. Della Fahria A - Ponorogo - SMAN 1 Ponorogo
41. Putri Puspita Sari - Magetan - SMAN 2 Madiun
42. Erni Kurnia Putri - Blitar - SMAN 1 Srengat
43. Maidatul Janah - Blitar - SMAN 3 Blitar
44. Dhitasya Salsabilla - Malang - SMAN 4 Malang
45. Ubaid firmandeka h - Sidoarjo - SMAN 1 TAMAN
46. Putri Agustiningrum - Blitar - SMAN 1 Talun
47. Damar S. - Malang - SMAN 1 Lawang
48. Anisa Hardiani Putri-Tulungagung-SMAN 1 Kedungwaru
49. Rayhan Abyasa - Mojokerto - SMAN 1 Sooko
50. Deanty Fatihatul Magfirah - Gresik - SMAN 15 Surabaya
51. Rizqi Annisa Zen - Madiun - SMAN 2 Madiun
52. Arnoldy Fatwa R. - Sidoarjo - SMAN 15 Surabaya
53. Muhammad Sirojuddin - Pasuruan - SMAN 1 PASURUAN
54. Niken Puspitasari - Kediri - SMAN 2 Pare
55. Lovidiaz Elsyfa Yessyratna Arif- Kediri-SMAN 2 Pare
56. Saffanah Afifah - Sidoarjo - SMAN 1 Sidoarjo
57. Sherina Rafidah Khairunnisa - Banyuwangi - SMAN 1 Glagah
58. Bayu Rayhan - Pasuruan - SMA Arrohmah Malang
59. Nurfi Ardillah S-Sidoarjo-MAN Sidoarjo
60. Septian Tegar Yuristiawan - Banyuwangi - SMAN 1 Glagah
61. Azzahra dhisa khamila - Banyuwangi - MAN 1 Banyuwangi
62. Yoqi Nanda Gustifanny - Banyuwangi - SMAN 1 Glagah
63. Putri Ulul Azmi - Probolinggo - MAN 2 Probolinggo
64. Mutiara Indryan Sari - Ngawi - SMAN 2 Madiun
65. Adisti Ziyadatul Ilmiyah - Sidoarjo - SMAN 1 Sidoarjo
66. Norvan Bagus Ramadhan - Ngawi - SMAN 2 Ngawi
67. aditya prameswara a - kediri - SMAN 1 KEDIRI
68. Jauharotul Maslakhah - Nganjuk - SMAN 1 Kertosono
69. Halimatus Sa'diyah - Pamekasan - SMAN 3 PAMEKASAN
70. Faiz Fanani - Gresik - SMAN 1 GRESIK
71. Anita Ambarsari - Ngawi - SMAN 2 Ngawi
72. Afriani Kartika Putri -Surabaya - SMAN 5 Surabaya
73. Ardiana Eka Prasiska - Sidoarjo - SMAN 1 Sidoarjo
74. Cannava Neira Cahaya Jalasveva - Pasirian - SMAN 4 Kulon Progo
75. Nisa Ayu Larasati - Jember - SMAN 1 Jember
76. M. Almas Yafi' - Probolinggo - SMA Taruna Dra. Zulaeha
77. Lailatul Hasanah - Pasuruan - SMAN 1 Purwosari
78. Dwi Ajeng P - Surabaya - SMAN 1 Surabaya
79. Arumia Mustika Rani - Lumajang - SMAN 2 Lumajang
80. Shashella zelicha - Lumajang - SMAN 2 Lumajang
81. Nurfi Kurnia Afrida - Sidoarjo - SMAN 1 Sidoarjo
82. Yustika - Banyuwangi - SMAN 1 Genteng
83. Imam Sujono - Tulungagung - SMAN 1 Kedungwaru
84. Laili fatqulia rahma - Sidoarjo - SMAN 3 Sidoarjo
84. Maulana Pandudinata - Surabaya - SMAN 2 Surabaya
85. Syafni Dwi Darmawan - Sumenep - SMAN 1 Sumenep"


# olah text ---------------------------------------------------------------
s %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble_col(column_name = "nama") %>% 
  separate(nama, c("nama", "kota", "asal_sekolah"), sep = "-") %>% 
  mutate(nama = str_remove_all(nama, "\\d|[:punct:]"),
         kota = str_trim(kota),
         asal_sekolah = str_trim(asal_sekolah)) %>% 
  filter((row_number() == 21) | (between(row_number(), 1, 6)))
as.data.frame()


# contoh ------------------------------------------------------------------

s <- '1. Aceh 2 349 3 746
2. Sumatera Utara 6 189 5 408
3. Sumatera Barat 3 009 2 960
4. Riau 1 734 1 446
5. Jambi 1 221 1 208
6. Sumatera Selatan 1 219 1 579
7. Bengkulu 658 623
8. Lampung 2 188 2 080
9. Kep. Bangka Belitung 275 326
10. Kepulauan Riau 776 879
11. DKI Jakarta 5 883 8 277
12. Jawa Barat 7 602 7 685
13. Jawa Tengah 19 191 24 937
14. Yogyakarta 4 728 5 806
15. Jawa Timur 24 757 25 622
16. Banten 1 499 1 418
17. Bali 2 847 3 131
18. Nusa Tenggara Barat 1 689 1 718
19. Nusa Tenggara Timur 1 678 1 408
20. Kalimantan Barat 1 183 910
21. Kalimantan Tengah 842 930
22. Kalimantan Selatan 487 781
23. Kalimantan Timur 723 553
24. Kalimantan Utara 181 153
25. Sulawesi Utara 2 139 1 836
26. Sulawesi Tengah 1 831 1 568
27. Sulawesi Selatan 7 547 5 940
28. Sulawesi Tenggara 1 019 937
29. Gorontalo 515 371
30. Sulawesi Barat 610 484
31. Maluku 408 319
32. Maluku Utara 271 202
33. Papua Barat 475 805
34. Papua 1 492 365'

data.frame(s = str_split(s, '\n')[[1]]) %>% 
  extract(s, into = c('a', 'b', 'c', 'd'),
          regex = '^(\\d+)\\.\\s(\\D+)\\s(\\d{1,3}\\s\\d+|\\d+)\\s(\\d{1,3}\\s\\d+|\\d+)') %>% 
  mutate_at(3:4, ~ gsub('\\s', '', .x)) %>% 
  type_convert() %>% 
  select(-a) %>% 
  mutate(p = (d-c)*100/c)

# Digit koma, -, tanpa angka depan
# -?\\d*?\\.\\d*
# Kelmahan - 111.2 jika terpisah spasi atau pakai ,

str_extract_all('aaa -.12 -1.000 111 1.2 -111 -111.2',
                '(-?\\d*?\\.\\d*|-?\\d+)')