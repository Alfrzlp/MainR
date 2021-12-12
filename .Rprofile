# clearscreen
cat("\014")
cat('Bismillah,\n')
# Customise the R prompt that prefixes every command
# (use " " for a blank prompt)

options(prompt = "R> ", show.signif.stars = F)

if(interactive()){
  try(message('Hi Ridson, welcome to R... \n\n\n'), silent = TRUE)
  # message(sample(nasehat, 1), '\n\n')
  suppressMessages(require(devtools))
  suppressWarnings(require(tidyverse))
}


# setwd("~/Main R")

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

.env$str2vec <- function(str, adakoma = T, pemisah = ' '){
  str = gsub(",", ".", str)
  str = gsub(paste0(pemisah, "|\t|\n"), " ", str)
  str = strsplit(str, split = " ")[[1]]
  # ambil yang ada angka dan ubah jadi numeric
  vec = as.numeric(str[stringr::str_detect(str, "[:alnum:]")])
  return(vec)
}

attach(.env)
