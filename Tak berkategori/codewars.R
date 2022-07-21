library(tidyverse)

# -------------------------------------------------------------------------
movie <- function(card, ticket, perc) {
  val <- new_val <- ticket * perc
  i <- 1
  total <- card + val
  while (ceiling(total) >= i*ticket) {
    i <- i + 1
    new_val <- val * perc
    total <- total + new_val
    val <- new_val
  }
  return(i)
}

movie(500, 15, 0.9)
movie(100, 10, 0.95)
movie(0, 10, 0.95)

# -------------------------------------------------------------------------
dec2FactString <- function(nb) {
  res <- c()
  i <- 1
  val <- nb
  while (val >= 1) {
    code <- val %% i
    res <- c(res, ifelse(code >= 10, LETTERS[code - 9], as.character(code)))
    val <- val %/% i
    print(paste(val, " ", val %% i))
    i <- i + 1
  }
  res <- paste0(rev(res), collapse = '')
  res
}


factString2Dec <- function(str) {
  all_letter <- as.character(10:35)
  names(all_letter) <- LETTERS
  
  x <- strsplit(str, split = '')[[1]]
  x <- as.numeric(str_replace_all(x, all_letter))
  sum(x * factorial(length(x):1 - 1))
}

dec2FactString(2982)
dec2FactString(463)
dec2FactString(36288000)
dec2FactString(7890123456)

factString2Dec('4041000')
factString2Dec('341010')
factString2Dec('A0000000000')
factString2Dec('76A0000021000')
factString2Dec('7890123456')




# -------------------------------------------------------------------------
doubles <- function(maxk, maxn) {
  sum(sapply(1:maxk, function(k) sum(1 / (k * (1:maxn + 1)^(2*k)))))
}

doubles(1, 3) 
#0.4236111111111111
doubles(1, 10)
#0.5580321939764581
doubles(10, 100)
#0.6832948559787737


# -------------------------------------------------------------------------
dblLinear <- function (n) {
  new_val <- x <- 1
  while(length(unique(x)) < n){
    val <- c(2*new_val+1, 3*new_val+1)
    x <- c(x, val) 
    new_val <- val
  }
  unique(sort(x))[n + 1]
}

dblLinear(10) #22
dblLinear(20) #57
dblLinear(166466)
dblLinear(166464)

i <- res <- 166460
while(res < 7202134){
  res <- ifelse(is.na(dblLinear(i)), 0, dblLinear(i))
  print(res)
  i <- i + 1
}
i
