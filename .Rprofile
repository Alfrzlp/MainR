# clearscreen
cat("\014")
# Customise the R prompt that prefixes every command
# (use " " for a blank prompt)

options(prompt = "~> ", show.signif.stars = F)

if (interactive()) {
  suppressMessages(require(devtools))
  suppressMessages(require(tidyverse))

  jam <- as.numeric(str_extract(Sys.time(), pattern = "\\s\\d+"))
  salam <-
    case_when(
      between(jam, 1, 10) ~ "Selamat Pagi",
      between(jam, 10, 15) ~ "Selamat Siang",
      between(jam, 16, 18) ~ "Selamat Sore",
      between(jam, 19, 24) ~ "Selamat Malam"
    )
  cat(salam, "Ridson..")
  message("\nSelamat datang di R\n")
  rm(salam, jam)
}
