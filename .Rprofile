# clearscreen

# Customise the R prompt that prefixes every command
# (use " " for a blank prompt)



if (interactive()) {
  cat("\014")
  # suppressMessages(require(devtools))
  suppressMessages(suppressWarnings(require(tidyverse)))
  suppressMessages(suppressWarnings(require(crayon)))
  suppressMessages(suppressWarnings(require(grDevices)))
  
  jam <- as.numeric(str_extract(Sys.time(), pattern = "\\s\\d+"))
  salam <-
    case_when(
      between(jam, 1, 10) ~ "Selamat Pagi",
      between(jam, 10, 15) ~ "Selamat Siang",
      between(jam, 16, 18) ~ "Selamat Sore",
      between(jam, 19, 24) ~ "Selamat Malam"
    )
  lime <- make_style(rgb(94, 201, 98, maxColorValue = 255), bg = F)
  ivory <- make_style("ivory")
  cat(bold(lime(ivory(salam, "Ridson..."))))
  cat(bold("\nSelamat datang di R\n\n"))
  
  cat(lime(italic('tidyverse :')), ivory('oke'), '\n')
  tidyverse_conflicts()
  rm(salam, jam)
  source('awal.R')
}


