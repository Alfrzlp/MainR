c(17676, 19191, 24937)

(24495-24937)*100/24937
(24937-19191)*100/19191
(19191-17676)*100/19191

(25622-24757)*100/24757

(110-100)/100


c <- '1. Aceh 2 349 3 746
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
34. Papua 1 492 365
'

data.frame(x = strsplit(c, split = '\n')[[1]]) %>% 
  separate(x, )

str_split('1. Aceh 2 349 3 746')
data.frame(nama = c("D:apalah ini kalau/bahan-kue/puding/Aceh.csv")) %>% 
  extract(nama, into = c("a", "b", "c"),
          regex = ".*/(.*)/(.*)/(.*)\\.csv$",
          remove = F)
