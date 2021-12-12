# fungsi untuk resize dan dapatkan gambar & label
image_from_directory = function(alamat, format, w=NA, h=NA){
  dir = list.files(alamat)
  label = c()
  image = c()
  
  for(i in 1:length(dir)){
    isi = list.files(paste0(alamat,"/",dir[i]), full.names = T, recursive = T)
    
    label = c(label, length(isi))
    # convert semua gambar sesuai format
    for(j in 1:length(isi)){
      img = image_convert(image_read(isi[j]), format = format)
      img = image_resize(img, paste0(w,"x",h))
      image_write(img, paste0(isi[j], ".",format))
      file.remove(isi[j])
      # print(paste(j, "done"))
      image = c(image, paste0(isi[j], ".",format))
    }
    print(paste(dir[i], "done"))
  }
  
  image = lapply(image, readImage)
  label = rep(0:(length(dir)-1), label)
  
  return(list(image, label))
}


#----------------------------
# baca dan proses image
image_preprocessor <- function(alamat, ...){
  image = array(dim = c(1, 32, 32, 3))
  for(i in 1:length(alamat)){
    img = image_load(alamat[i], target_size = c(32,32), ...)
    img = image_to_array(img)
    img = array_reshape(img, c(1, dim(img)))
    img = img/255
    image = abind(image, img, along = 1)
  }
  return(image[-1, , ,])
}

# karena aku gabisa pakai fungsi aperm dari EBImage
# maka fungsi ini alternatifnya
# mendapatakan semua array gambar dari directory
# dan label berdasarkan sub directory
image_from_directory = function(alamat, w, h, ...){
  dir = list.files(alamat)
  label = c(); allloc = c()
  
  for(i in 1:length(dir)){
    isi = list.files(paste0(alamat,"/",dir[i]), full.names = T, recursive = T)
    label = c(label, length(isi))
    allloc = c(allloc, isi)
  }
  label = rep(0:(length(dir)-1), label)
  
  image = array(dim = c(1, w, h, 1))
  for(i in 1:length(allloc)){
    img = image_load(allloc[i], target_size = c(w, h), ...)
    img = image_to_array(img)
    img = array_reshape(img, c(1, dim(img)))
    img = img/255
    image = abind(image, img, along = 1)
  }
  
  return(list(image[-1, , , ,drop = F], label))
}

# pararel-------------------------
# with pararel iteration
get_all_loc = function(alamat){
  dir = list.files(alamat)
  label = c(); allloc = c()
  
  cores = detectCores()
  # agar tidak over
  cl <- makeCluster(cores-1)
  registerDoParallel(cl)
  
  info = foreach(i = 1:length(dir), .combine = c) %dopar% {
    isi = list.files(paste0(alamat,"/",dir[i]), full.names = T, recursive = T)
    
    list(loc = c(isi, allloc), label = c(label, length(isi)))
  }
  #info[[2]] = rep(0:(length(dir)-1), info[[2]])
  
  stopCluster(cl)
  return(info)
}



image_preprocessor <- function(allloc, w, h, grayscale = T){
  cores = detectCores()
  # agar tidak over
  cl <- makeCluster(cores-1)
  registerDoParallel(cl)
  abind1 = function(...) abind(..., along = 1)
  
  # iterasi secara pararel
  array <- foreach(i = 1:length(allloc), .combine = "abind1",
                   .packages = c("keras", "abind"), .multicombine = T) %dopar% {
                     img = image_load(allloc[i], target_size = c(w, h), grayscale = grayscale)
                     img = image_to_array(img)
                     img = array_reshape(img, c(1, dim(img)))
                     img = img/255
                     
                     img
                   }
  stopCluster(cl)
  return(array)
}




# ebimage-----------------------------------------------------------
library(EBImage)
image_from_directory <- function(alamat, w, h, grayscale = T){
  start = Sys.time()
  dir = list.files(alamat)
  allloc = c(); label = c()
  
  # mendapatkan semua lokasi gambar
  if(str_detect(dir[1], "(png|jpg|jpeg)")) allloc = dir
  else{
    for(i in 1:length(dir)){
      isi = list.files(paste0(alamat,"/",dir[i]), full.names = T, recursive = T)
      allloc = c(allloc, isi)
      label = c(label, rep(dir[i], length(isi)))
    }
  }
  # make progress bar
  pb <- txtProgressBar(1, length(allloc), style = 3)
  
  images = lapply(allloc, readImage) 
  for (i in 1:length(allloc)){
    images[[i]] <- resize(images[[i]], w, h)
    images[[i]] <- toRGB(images[[i]])
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # gabung semua image
  images = combine(images)
  # ubah urutan dimensi 
  images = aperm(images, c(4, 1, 2, 3))
  cat("Time  :",Sys.time()-start)
  return(list(images, label))
}



# pindah gambar ---------------------------------------------------------
loc = "D:/Datasets/CNN/a/train"
loc2 = "D:/Datasets/CNN/b"
library(magick)

pindah <- function(loc, loc2, format = "png"){
  dir = list.files(loc, full.names = T)
  dir2 = list.files(loc2, full.names = T)
  
  for(i in 1:length(dir)){
    loc_gambar = list.files(dir2[i], full.names = T)
    n = 0
    pb <- txtProgressBar(1, length(loc_gambar), style = 3)
    
    for(j in 1:length(loc_gambar)){
      tryCatch({
        img = image_read(loc_gambar[j])
        n = n + 1
      },
      error = function(a) print(paste("error file", loc_gambar[j])))
      
      img = image_convert(img, format = format)
      image_write(img, paste0(dir[i], "/", list.files(loc)[i],"_",j, ".",format))
      setTxtProgressBar(pb, j)
    }
    close(pb)
    cat(dir[i], " done\n")
    cat(n, "sukses dari ",length(loc_gambar), "\n\n")
  }
}

pindah(loc, loc2)