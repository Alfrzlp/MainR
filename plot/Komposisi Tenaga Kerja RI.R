str <- ",1.133.546 	 2.819.380 	 7.866.785 	 4.231.994 	 2.889.498 	 1.229.398 	 382.209 	 368.757 
 1.792.265 	 3.902.901 	 9.198.696 	 3.334.819 	 2.130.922 	 849.185 	 289.151 	 275.055 
 78.662 	 265.055 	 854.386 	 605.663 	 678.830 	 260.379 	 89.451 	 182.900 
 293.097 	 1.417.260 	 5.956.537 	 5.343.171 	 6.355.636 	 3.619.241 	 1.947.943 	 3.250.888 
 614.450 	 1.381.467 	 3.025.685 	 724.250 	 191.472 	 51.714 	 1.722 	 733 
 168.193 	 662.248 	 2.529.777 	 1.207.662 	 456.553 	 244.416 	 12.298 	 11.115 
 1.387.917 	 2.660.520 	 7.325.087 	 3.591.634 	 1.693.952 	 502.000 	 149.094 	 65.131 
"

df <- read.table(textConnection(str)) %>%
  mutate_all(~ str_replace_all(.x, "\\.", "")) %>%
  mutate_all(~ str_remove_all(.x, "\\,")) %>%
  type_convert() %>%
  `colnames<-`(c(
    "Tidak/belum pernah sekolah", "Tidak/belum tamat SD ", "SD", "SLTP", "SLTA Umum/SMU",
    "SLTA Kejuruan/SMK", "Akademi/Diploma", "Universitas"
  ))

df$status <- c(
  "Berusaha Sendiri",
  "Berusaha dibantu Buruh Tidak Tetap/Tidak Dibayar",
  "Berusaha dibantu Buruh Tetap/Dibayar",
  "Buruh/Karyawan/Pegawai",
  "Pekerja bebas pertanian",
  "Pekerja bebas non pertanian",
  "Pekerja keluarga/tak dibayar"
)

df %>%
  pivot_longer(1:8) %>%
  mutate(name = factor(name, levels = colnames(df))) %>%
  group_by(status) %>%
  mutate(rank = sum(value)) %>%
  ggplot(aes(fill = name, x = value, y = reorder(status, rank))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    fill = "Pendidikan Tertinggi yang di tamatkan", x = "Jumlah Penduduk", y = "Status Pekerjaan",
    title = "Status Pekerjaan Utama dan Pendidikan tertinggi yang ditamatkan",
    subtitle = "Agustus 2008",
    caption = my_caption
  ) +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    legend.key.height = unit(0.05, "cm")
  )

ggsave("D:/tk8.png", width = 25, height = 15, units = "cm")
