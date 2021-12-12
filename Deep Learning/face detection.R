library(opencv)
library(psych)

ocv_video(ocv_face)

#variasi
ocv_video(ocv_edges)
ocv_video(ocv_knn)
#ocv_video(ocv_facemask)
ocv_video(ocv_mog2)
#ocv_video(ocv_stylize)
#ocv_video(ocv_sketch)

#ambil gambar
test <- ocv_picture()
bitmap <- ocv_bitmap(test)
str(bitmap)
width <- dim(bitmap)[2]
height <- dim(bitmap)[3]

png('D:/bg.png',width = width, height = height)


pic <- ocv_read("D:/Screenshot 2020-10-09 153843.jpg")

#mendektedi wajah
face <- ocv_face(pic)
face
#menyimpan bulatan
save1 <- ocv_write(face1,'opcv1.png')
