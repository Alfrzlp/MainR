if (interactive()) {
  cat("\014")
  extrafont::loadfonts(device = "win")
  suppressMessages(suppressWarnings(require(tidyverse)))
  
  jam <- as.numeric(str_extract(Sys.time(), pattern = "\\s\\d+"))
  salam <-
    case_when(
      between(jam, 1, 10) ~ "Selamat Pagi",
      between(jam, 10, 15) ~ "Selamat Siang",
      between(jam, 16, 18) ~ "Selamat Sore",
      between(jam, 19, 24) ~ "Selamat Malam"
    )
  # lime <- make_style(rgb(94, 201, 98, maxColorValue = 255), bg = F)
  options(prompt = '~> ')
  
  cli::cli_div(
    theme = list(
      span.emph = list(color = "#5EC962"),
      span.bold = list("font-weight" = "bold", color = 'cyan')
    )
  )
  
  cli::cli_h1('{salam} Ridson')
  cli::cli_alert_success('{.bold Tidyverse} : Sudah diload')
  cli::cli_alert_success('{.emph  Selamat Beraktivitas...}')
  cli::cli_blockquote('Jangan malas', citation = 'Ridson')
  
  cli::cli_end()
}


