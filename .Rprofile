# clearscreen
cat("\014")
# Customise the R prompt that prefixes every command
# (use " " for a blank prompt)

options(prompt = "R> ", show.signif.stars = F)

if(interactive()){

  suppressMessages(require(devtools))
  suppressMessages(require(tidyverse))
  
  jam <- as.numeric(str_extract(Sys.time(), pattern = '\\s\\d+'))
  salam <- 
    case_when(
      between(jam, 1, 10) ~ 'Selamat Pagi', 
      between(jam, 10, 15) ~ 'Selamat Siang', 
      between(jam, 16, 18) ~ 'Selamat Sore', 
      between(jam, 19, 24) ~ 'Selamat Malam' 
    )
  cat(salam, 'Ridson..')
  message('\nSelamat datang di R\n')
}



.env = new.env()
# menjadi fungsi bawaan
.env$ht = function(d, n = 5) rbind(head(d, n), tail(d, n))
.env$nice_par = function(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -0.01,
                         cex.axis = 0.9, las = 1, mfrow = c(1, 1), ...) {
  par(mar = mar, mgp = mgp, tck = tck, cex.axis = cex.axis, las = las,
      mfrow = mfrow, ...)
}

.env$copy2c <- function(df, dec = ',', rownames = F, colnames = F, ...) {
  write.table(df, 'clipboard', dec = dec, row.names = rownames,
              col.names = colnames, sep = '\t',)
}



attach(.env)
