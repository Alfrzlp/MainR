str2vec <- function(str, adakoma = T, pemisah = "\\s") {
  str <- gsub(",", ".", str)
  str <- gsub(paste0(pemisah, "|\t|\n"), " ", str)
  str <- str_split(str, pattern = " ")[[1]]
  # ambil yang ada angka dan ubah jadi numeric
  vec <- as.numeric(str[str_detect(str, "[:alnum:]")])
  return(vec)
}
