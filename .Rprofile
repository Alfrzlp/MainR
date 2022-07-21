if (interactive()) {
  cat("\014")
  options(tidymodels.dark = TRUE)
  extrafont::loadfonts(device = "win")
  # suppressMessages(suppressWarnings(require(tidyverse)))
  
  jam <- as.numeric(format(Sys.time(), '%H'))
  salam <- cut(jam, breaks =  c(1, 10, 15, 16, 18), labels = c('Pagi', 'Siang', 'Sore', 'Malam'))
  # lime <- make_style(rgb(94, 201, 98, maxColorValue = 255), bg = F)
  options(prompt = '~> ')
  
  cli::cli_div(
    theme = list(
      span.emph = list(color = "#5EC962"),
      span.bold = list("font-weight" = "bold", color = 'cyan')
    )
  )
  
  cli::cli_h1('Selamat {salam} Ridson')
  cli::cli_alert_success('{.bold Tidyverse} : Sudah diload')
  cli::cli_alert_success('{.emph  Selamat Beraktivitas...}')
  cli::cli_blockquote('Jangan malas', citation = 'Ridson')
  
  cli::cli_end()
}


