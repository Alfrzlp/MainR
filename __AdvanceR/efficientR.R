library(microbenchmark)

df <- iris
microbenchmark(
  df[1, 3],
  df$Sepal.Length[3], # tercepat
  df[1, "Sepal.Length"]
)


x <- 1:100 # initiate vector to cumulatively sum
# Method 1: with a for loop (10 lines)
cs_for <- function(x) {
  for (i in x) {
    if (i == 1) {
      xc <- x[i]
    } else {
      xc <- c(xc, sum(x[1:i]))
    }
  }
  xc
}

# Method 2: with apply (3 lines)
cs_apply <- function(x) {
  sapply(x, function(x) sum(1:x))
}

# Method 3: cumsum (1 line, not shown)
microbenchmark(times = 1, cs_for(x), cs_apply(x), cumsum(x))
devtools::install_github("csgillespie/efficient")

# yang dieksekusi setiap awal
file.edit("~/.Rprofile")
options(prompt = "R> ", digits = 4, show.signif.stars = F, continue = "  ")


good <- try(1 + 1, silent = TRUE)
bad <- try(1 + "1", silent = TRUE)
if (class(bad) == "try-error") print("a")


inst <- lapply(pkgs, library, character.only = TRUE)




# shiny -------------------------------------------------------------------

devtools::install_github("rstudio/profvis")

library(profvis)

profvis({
  plot(carat ~ price, diamonds)
})

expensive_function <- function(x) {
  sum((1:x)^2)
  Sys.sleep(5) # make it seem to take even longer
}

library(memoise)
memoised_expensive_function <- memoise(expensive_function)

system.time(memoised_expensive_function(1000)) # Takes at least 5 seconds
system.time(memoised_expensive_function(1000)) # Returns much faster
