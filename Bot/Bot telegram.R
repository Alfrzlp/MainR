library(telegram.bot)
bot <- Bot(token = "1428324784:AAGr6Li8WLb0VRvBERHXdjzbURy-LmJ5044")
file.edit(path.expand(file.path("~", ".Renviron")))

updater <- Updater(token = bot_token("RTelegramBot"))
updater <- Updater(token = "1428324784:AAGr6Li8WLb0VRvBERHXdjzbURy-LmJ5044")

print(bot$get_me())


start <- function(bot, update) {
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = sprintf("belajar %s!", update$message$from$first_name)
  )
}
updater <- updater + CommandHandler("start", start)





echo <- function(bot, update) {
  bot$sendMessage(chat_id = update$message$chat_id, text = "update$message$text")
}
updater <- updater + MessageHandler(echo, MessageFilters$text)




caps <- function(bot, update, args) {
  if (length(args > 0L)) {
    text_caps <- toupper(paste(args, collapse = " "))
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps
    )
  }
}
updater <- updater + CommandHandler("caps", caps, pass_args = TRUE)






unknown <- function(bot, update) {
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = "Sorry, I didn't understand that command."
  )
}
updater <- updater + MessageHandler(unknown, MessageFilters$command)




balas <- function(bot, update) {
  if (update$message$text == "sayang") {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Hay Sayang")
  } else {
    bot$sendMessage(chat_id = update$message$chat_id, text = "ngomong sing jelas talah")
  }
}
updater <- updater + MessageHandler(balas, MessageFilters$text)



# Example of a 'kill' command
kill <- function(bot, update) {
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = "Bye!"
  )
  # Clean 'kill' update
  bot$getUpdates(offset = update$update_id + 1L)
  # Stop the updater polling
  updater$stop_polling()
}
updater <- updater + CommandHandler("kill", kill)


# start bot
updater$start_polling()













# Send message
bot$sendMessage(chat_id,
  text = "foo *bold* _italic_",
  parse_mode = "Markdown"
)

# Send photo
bot$sendPhoto(chat_id,
  photo = "https://telegram.org/img/t_logo.png"
)

# Send audio
bot$sendAudio(chat_id,
  audio = "http://www.largesound.com/ashborytour/sound/brobob.mp3"
)

# Send document
bot$sendDocument(chat_id,
  document = "https://github.com/ebeneditos/telegram.bot/raw/gh-pages/docs/telegram.bot.pdf"
)

# Send sticker
bot$sendSticker(chat_id,
  sticker = "https://www.gstatic.com/webp/gallery/1.webp"
)

# Send video
bot$sendVideo(chat_id,
  video = "http://techslides.com/demos/sample-videos/small.mp4"
)

# Send gif
bot$sendAnimation(chat_id,
  animation = "https://media.giphy.com/media/sIIhZliB2McAo/giphy.gif"
)

# Send location
bot$sendLocation(chat_id,
  latitude = 51.521727,
  longitude = -0.117255
)

# Send chat action
bot$sendChatAction(chat_id,
  action = "typing"
)

# Get user profile photos
photos <- bot$getUserProfilePhotos(user_id = chat_id)

# Download user profile photo
file_id <- photos$photos[[1L]][[1L]]$file_id
bot$getFile(file_id, destfile = "photo.jpg")
