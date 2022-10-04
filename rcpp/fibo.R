
fiboR <- function(n){
  prev_val <- 0
  for (i in 1:n) {
    if (i <= 1) val = 1
    else {
      temp = val
      val = val + prev_val
      prev_val = temp
    }
  }
  return(val)
}

fiboR(100)
fiboCpp(100)


library(Rcpp)
library(microbenchmark)
library(tidyverse)
sourceCpp('C:/MainCpp/r/fibonaci.cpp')

res <- microbenchmark(
  fiboCpp(100),
  fiboR(100),
  check = 'equal',
  times = 1000
)
res
autoplot(res)

fiboCpp(100)
fiboR(100)




# -------------------------------------------------------------------------
sourceCpp('C:/MainCpp/r/splitdf.cpp')

res <- microbenchmark(
  cpp1 = meanCpp(seq(0.1, 10000, 0.23)),
  accumulate = meanCpp2(seq(0.1, 10000, 0.23)),
  reduce = meanCpp2(seq(0.1, 10000, 0.23)),
  r = mean(seq(0.1, 10000, 0.23)),
  times = 1000,
  check = 'equal',
  control = set.seed(1)
)
res
autoplot(res)
