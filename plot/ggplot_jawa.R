library(dplyr)

jawa <- read.csv("D:/Datasets/Pulau Jawa.csv")

#membuat lokasi nama kabupaten
nama <- jawa %>% group_by(kabupaten) %>% 
  summarise(long = mean(long), lat = mean(lat))
head(nama)
#===============================================================================
#plot pulau jawa
library(ggplot2)

ggplot(jawa, aes(x=long, y=lat)) + 
  geom_map(map=jawa, aes( map_id=id, fill=provinsi), color="white", size=0.5) +
  geom_text(aes(x=long, y=lat, label = kabupaten), data=nama, size=1.5) +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "none")

#===============================================================================
#Mengambil subset jawa timur
jawatimur <- jawa %>% filter(provinsi == "Jawa Timur")

#Membuat posisi nama kab jawa timur
kab_jatim <- jawatimur %>% group_by(kabupaten) %>% 
  summarise(long = mean(long), lat = mean(lat))

#Plot Jatim
ggplot(jawatimur, aes(x=long, y=lat)) + 
  geom_map(map=jawatimur, aes(map_id=id, fill=kabupaten), color="white", size=0.5) +
  geom_text(aes(x=long, y=lat, label = kabupaten), data=kab_jatim, size=3) +
  scale_fill_viridis_d(option = "D") +
  ylim(NA, -6.4) + xlim(NA, 116) + 
  theme(legend.position = "none")
#===============================================================================
#gabung dgn data hutan jawa timur
hutan <- read.csv("D:/Datasets/kebun_jatim.csv", sep = ";")

jawatimur <- left_join(jawatimur, hutan, by="kabupaten")
#membetulkan nama kolom
library(janitor)
jawatimur <- clean_names(jawatimur)
head(jawatimur)

#plot polygon luas hutan dan perairan
map = ggplot(jawatimur, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=jumlah_luas_hutan_dan_perairan), color = "transparent") +
  geom_text(aes(x=long, y=lat, label = kabupaten), data=kab_jatim, size=2) +
  labs(subtitle = "Dari data BPS Jawa Timur", fill="Luas Hutan & Perairan (Hektar)") +
  scale_fill_viridis_c(option = "D", direction = -1) +
  ggtitle("Jumlah Luas Hutan dan Perairan Provinsi Jawa Timur Tahun 2018") + coord_map()


map
ggplotly(map)
#===============================================================================
#Hutan Lindung
jawatimur <- jawatimur %>% mutate(hutan_lindung = as.integer(hutan_lindung))

#grup taruh di polygon jangan di ggplot()
ggplot(jawatimur, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=hutan_lindung), color = "white") +
  geom_text(data=kab_jatim, aes(x=long, y=lat, label = kabupaten), size=3) +
  labs(subtitle = "Dari data BPS Jawa Timur", fill="Luas Hutan Lindung (Hektar)") +
  ggtitle("Luas Hutan Lindung Provinsi Jawa Timur Tahun 2018") +
  scale_fill_viridis_c(option = "D", direction = -1) + coord_map()

