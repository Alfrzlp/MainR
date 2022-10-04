library(tidyverse)

# Estimasi Nilai Phi ------------------------------------------------------
n <- 10000
x <- runif(n, min = 0, max = 1)
y <- runif(n, min = 0, max = 1)

acc <- y <= sqrt(1 - x^2)
acc

est_phi <- 4 * sum(acc) / n
est_phi

# z <- x^2 + y^2
# acc <- n - sum(floor(z))

data.frame(x, y, acc) %>% 
  ggplot(aes(x = x, y = y, col = acc)) +
  geom_point() +
  scale_color_manual(values = c('#F57328', '#7FB77E')) +
  coord_equal()



# Improved Monte Carlo Integration ----------------------------------------
a <- 0
b <- 1
fx <- function(x) x^2 + x

n <- 1000000
x <- runif(n, min = a, max = b)
y <- fx(x)

# hasil integral
(b - a) * mean(y)


# Integral lebih kompleks -------------------------------------------------
n <- 10^6
a1 <- 0
b1 <- pi
a2 <- 0
b2 <- 2*pi
a3 <- 10
b3 <- 11

func <- function(x, y, z){
  9 * z^2 * sin(x)^3 / (2 * pi)
}

x <- runif(n, a1, b1)
y <- runif(n, a2, b2)
z <- runif(n, a3, b3)

fs <- func(x, y, z)

(b3 - a3) * (b2 - a2) * (b1 - a1) * mean(fs)



# Markov Chain ------------------------------------------------------------
# initial probability distribution 
# transition probability kernel

# probabilitas bahwa suatu proses di state space si bergerak 
# ke keadaan sj dalam satu langkah