library(telegram.bot)
library(stringr)
library(rSymPy)
library(image.darknet)
sympyStart()

# membuat semua huruf sebagai variable
for(i in letters) Var(i)
for(i in LETTERS) Var(i)

token = load_text_tokenizer("D:/Datasets/Model/new_gender")
model <- load_model_hdf5("D:/Datasets/Model/new_gender.h5")

# fungsi
text.to.token = function(string, max_len = 30){
  seq = texts_to_sequences(token, string)
  pad_sequences(seq, maxlen = max_len, padding = "post") %>% return()
}

# membuat updater dan bot
# bot <- Bot(token = "1426189822:AAH_lN8XLkrkJB48iXHPJwy3Y3KR7QqsglU")
# updater <- Updater(token = "1426189822:AAH_lN8XLkrkJB48iXHPJwy3Y3KR7QqsglU")

bot <- Bot(token = bot_token("RTelegramBot"))
updater <- Updater(token = bot_token("RTelegramBot"))

# Cek
print(bot$get_me())

# membuat fungsi start---------------------------------------------------
# hanya untuk menyapa
start <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Hai %s!, saya akan berusaha membantumu", update$message$from$first_name))
  # bot$sendAudio(chat_id = update$message$chat_id,
  #               audio = "C:/Users/Ridson Alfarizal/Documents/Main Python New/test.mp3")
  bot$send_voice(chat_id = update$message$chat_id,
                 voice = "C:/Users/Ridson Alfarizal/Documents/Main Python New/test.mp3")
}

updater <- updater + CommandHandler("start", start)


# membuat fungsi kill----------------------------------------------------
# untuk menghentikan bot
# kill <- function(bot, update){
#   bot$sendMessage(chat_id = update$message$chat_id,
#                   text = paste("See you", update$message$from$first_name), "...")
#   # Clean 'kill' update
#   bot$getUpdates(offset = update$update_id + 1L)
#   # Stop the updater polling
#   updater$stop_polling()
# }
# updater <- updater + CommandHandler("kill", kill)


# Fungsi balas ---------------------------------------------------------
balas <- function(bot, update){
  pesan = tolower(update$message$text)
  print(pesan)
  if(str_detect(pesan, "sayang")){
    bot$sendMessage(chat_id = update$message$chat_id, text = "iya Sayang ?")
  }else if(str_detect(pesan, "(hai|hay)")){
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("hai juga",update$message$from$first_name,"Apakah ada yang bisa saya bantu?"))
  }else if(str_detect(pesan, "(lapar|laper|makan)")){
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("jangan lupa makan ya",update$message$from$first_name,",apa mau aku suapin nih?"))
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text = "pus ojo aneh2") 
  }
}
updater <- updater + MessageHandler(balas, MessageFilters$text)


# Membuat fungsi integral ----------------------------------------------
integral <- function(bot, update, args){
  tryCatch({
    if(length(args) == 2){
      hasil = sympy(paste("integrate((",args[1],"),(",args[2],"))"))
    }else if(length(args) == 4){
      hasil = sympy(paste("integrate((",args[1],"),(",args[2],",",args[3],",",args[4],"))"))
    }else if(length(args) == 7){
      hasil = sympy(paste("integrate((",args[1],"),(",args[2],",",args[3],",",args[4],"), 
                    (",args[5],",",args[6],",",args[7],"))"))
    }else if(length(args) == 10){
      hasil = sympy(paste("integrate((",args[1],"), (",args[2],",",args[3],",",args[4],"), 
                    (",args[5],",",args[6],",",args[7],"), (",args[8],",",args[9],",",args[10],"))"))
    }else hasil = "Maaf terdapat kesalahan dalam input"
  },
  error = function(...) hasil = "Maaf terdapat kesalahan dalam input")
  bot$sendMessage(chat_id = update$message$chat_id, text = hasil) 
}
updater <- updater + CommandHandler("integral", integral, pass_args = TRUE)


# Membuat fungsi sum ----------------------------------------------
Sum <- function(bot, update, args){
  tryCatch({
    if(length(args) == 4){
      hasil = sympy(paste("sum((",args[1],"),(",args[2],",",args[3],",",args[4],"))"))
    }else if(length(args) == 7){
      hasil = sympy(paste("sum((",args[1],"),(",args[2],",",args[3],",",args[4],"), 
                    (",args[5],",",args[6],",",args[7],"))"))
    }else if(length(args) == 10){
      hasil = sympy(paste("sum((",args[1],"), (",args[2],",",args[3],",",args[4],"), 
                    (",args[5],",",args[6],",",args[7],"), (",args[8],",",args[9],",",args[10],"))"))
    }else hasil = "Maaf terdapat kesalahan input"
  },
  error = function(...) hasil = "Maaf terdapat kesalahan input")
  bot$sendMessage(chat_id = update$message$chat_id, text = hasil) 
}
updater <- updater + CommandHandler("sum", Sum, pass_args = TRUE)



# Membuat fungsi limit ----------------------------------------------
limit <- function(bot, update, args){
  tryCatch({
    if(length(args) == 3) hasil = sympy(paste("limit(",args[1],",",args[2],",",args[3],")"))
    else hasil = "Maaf terdapat kesalahan input"
  },
  error = function(a) hasil = "Maaf terdapat kesalahan input")
  bot$sendMessage(chat_id = update$message$chat_id, text = hasil) 
}
updater <- updater + CommandHandler("limit", limit, pass_args = TRUE)


# Fungsi Help -------------------------------------------------------
help <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "contoh penggunaan
                  /integral fungsi x 0 1 y 1 oo
                  /limit x**2 x 1
                  /sum 3*x**2 x 2 10")
}
updater <- updater + CommandHandler("help", help)

# nama----------------------------------------------------------------
nama <- function(bot, update, args){
  id = update$message$chat_id
  
  name = args[1]
  for(i in 2:length(args)){
    nama = paste(name, args[i])
  }
  
  hasil = model %>% predict_proba(text.to.token(name))
  text = paste0("Perempuan  :  ", round(hasil[1]*100, 2), "%")
  text2 = paste0("Laki-laki  :  ", round(hasil[2]*100, 2), "%")
  
  bot$sendMessage(chat_id = id, text = text) 
  bot$sendMessage(chat_id = id, text = text2) 
}
updater <- updater + CommandHandler("nama", nama, pass_args = TRUE)


# fungsi gambar---------------------------------------------------
yolo_tiny_voc <- image_darknet_model(type = "detect", 
                                     model = "tiny-yolo-voc.cfg", 
                                     weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
                                     labels = system.file(package="image.darknet", "include", "darknet", "data", "voc.names"))

yolo <- function(bot, update){
  id = update$message$chat_id
  tryCatch({
    rlist::list.save(update$message$photo, "img.json")
    id = jsonlite::fromJSON("img.json")
    str = id$file_id[[1]]
    
    bot$getFile(str, destfile = "photo.jpg")
    
    x <- image_darknet_detect(file = paste0(getwd(),"/","photo.jpg"),
                              object = yolo_tiny_voc,
                              threshold = 0.19)
  },
  error = function(a) print("error"))
  bot$sendPhoto(chat_id = id, photo = "predictions.png")
}
updater <- updater + MessageHandler(yolo, MessageFilters$image)

# harus akhir
# Menghandle unknown function-------------------------------------------
unknown <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Maaf saya tidak mengerti perintah tersebut")
}
updater <- updater + MessageHandler(unknown, MessageFilters$command)



# start bot
updater$start_polling()






x = jsonlite::fromJSON("img.json")
class(x)
str = x$file_id[[1]]
str



str = "x x 1"
args = str_split(str, pattern = "\\s")[[1]]
args

hasil = sympy(paste("integrate((",args[1],"),(",args[2],"))"))
hasil = sympy(paste("integrate((",args[1],"),(",args[2],",",args[3],",",args[4],"))"))
hasil = sympy(paste("integrate((",args[1],"),
                    (",args[2],",",args[3],",",args[4],"), 
                    (",args[5],",",args[6],",",args[7],"), (",args[8],",",args[9],",",args[10],"))"))
hasil

args

x = ""
for(i in 1:length(args)) x = paste0(x,",",args[i])
x
paste("diff(x*y*z*c*v",x,")")
sympy(paste("diff(z*c*v",x,")"))
sympy(paste("diff(z*c*v",",c,v,z",")"))

str_detect("hai sayang", "sayang")


hasil = sympy(paste("limit(",args[1],",",args[2],",",args[3],")"))
hasil = sympy(paste("limit(",args[1],", ",args[2],", ",args[3],")"))
hasil
