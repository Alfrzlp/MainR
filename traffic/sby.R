# membuat 1 plot dan save
data = get_resume(file[26])
map = plot_leaflet(data, lng = 112.548564, lat = -7.316804, zoom = 11)
mapshot(map, file = "D:/jatim.png")


# membuat beberapa plot 
file = list.files("D:/Datasets/surabaya traffic", full.names = T)
nama_file = list.files("D:/Datasets/surabaya traffic") %>% 
  gsub("json", "png", .)

alamat = file.path(getwd(), "Main R/traffic/img sby")



for(i in 43:length(file)){
  
  df = get_resume(file[i])
  map <- plot_leaflet(df, lng = 112.548564, lat = -7.316804, zoom = 11)
  # save gambar langsung
  start <- Sys.time()
  mapshot(map, file = file.path(alamat, nama_file[i]))
  cat("Waktu untuk save  :", Sys.time() - start, "\n")
}


# jadikan gif
all_loc <- list.files("Main R/traffic/img sby", full.names = T)
hari <-  c("S e l a s a", "R a b u")
waktu <- ymd_hms("2021-02-23 00:00:00") 
tgl_start = day(waktu)
img_list = list()
j = 0

for(i in 1:length(all_loc)){
  jam <- ifelse(nchar(hour(waktu))==1, paste0("0",hour(waktu)), hour(waktu))
  menit <- ifelse(nchar(minute(waktu))==1, paste0("0",minute(waktu)), minute(waktu))
  
  image <- image_read(all_loc[i])
  # write time
  image_text <- image_annotate(image, paste0(jam, ":", menit), color = "white", size = 40,
                               weight = 700, location = paste0("+",j%%864 + 10,"+630"))
  j <- j + 18
  # write day
  # karena dimulai hari sabtu
  image_text <- image_annotate(image_text, hari[day(waktu)%%tgl_start + 1], color = "gray50", size = 20,
                               location = "+10+670", weight = 700)
  # write date
  date = paste(day(waktu), "Februari", year(waktu))
  image_text <- image_annotate(image_text, date, color = "gray", size = 20,
                               location = "+10+690", weight = 700)
  
  # tambah 30 menit
  if(hour(waktu) == 21 & minute(waktu) == 30) waktu <- waktu + dminutes(60)
  else waktu <- waktu + dminutes(30)
  # tambahkan ke list
  img_list <- append(img_list, image_text)
}


imgs <- image_join(img_list)
gif <- image_animate(imgs, 5, loop = 1)
# simpan
image_write_gif(gif, "D:/surabaya_.gif")

18*48


image_read(all_loc[1])
