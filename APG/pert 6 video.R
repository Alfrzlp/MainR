s <- "6 27 25 15
6 23 28 13
18 64 36 22
8 44 35 29
11 30 15 31
34 75 44 64
28 26 42 30
71 124 54 64
43 54 34 56
33 30 29 20
20 14 39 21"

dat <-
  dm::read_string(s) %>%
  `colnames<-`(rep(c("bod", "ss"), 2))

comlab <- dat[, 1:2]
statelab <- dat[, 3:4]

D <- comlab - statelab
n <- nrow(D)
p <- ncol(D)
dbar <- 1 / n * t(D) %*% rep(1, n)
Sd <- var(D)

T2 <- n * t(dbar) %*% solve(Sd) %*% dbar
T2
(ftabel <- qf(0.05, p, n - p, lower.tail = F))
c2 <- (n - 1) * p * ftabel / (n - p)
c2

ifelse(T2 > c2, "Tolak Ho", "Gagal Tolak H0")[1]
# Tolak Ho artinya minimal terdapat salah satu variabel (x1 atau x2)
# yang beda antara commercial lab dengan state lab

a <- c(1, 0)
# simultan confidence interval
t(a) %*% dbar - sqrt(c2 * t(a) %*% Sd %*% a / n)
t(a) %*% dbar + sqrt(c2 * t(a) %*% Sd %*% a / n)
# melewati 0 maka delta = 0

# bonferoni (lebih pendek)
(ttabel <- qt(1 - 0.05 / (2 * p), n - 1))
t(a) %*% dbar - ttabel * sqrt(t(a) %*% Sd %*% a / n)
t(a) %*% dbar + ttabel * sqrt(t(a) %*% Sd %*% a / n)


# Pengukuran Berulang -----------------------------------------------------
dm::read_img()
s1 <- '426 609 556 600
253 236 392 395
359 433 349 357
432 431 522 600
405 426 513 513
324 438 507 539
310 312 410 456
326 326 350 504
375 447 547 548
286 286 403 422
349 382 473 497
429 410 488 547
348 377 447 514
412 473 472 446
347 326 455 468
434 458 637 524
364 367 432 469
420 395 508 531
397 556 645 625'

dat <- 
  dm::read_string(s1) %>% 
  `colnames<-`(paste0('tr', 1:4))

C <- matrix(c(-1, -1, 1, 1, 1, -1, 1, -1, 1, -1, -1, 1), nrow = 3, byrow = T)
C

n <- nrow(dat)
p <- ncol(dat)
xbar <- 1 / n * t(dat) %*% rep(1, n)
S <- var(dat)
alpha <- 0.05

T2 <- n * t(C %*% xbar) %*% solve(C %*% S %*% t(C)) %*% (C %*% xbar)
T2
(ftabel <- qf(alpha, p - 1, n - p + 1, lower.tail = F))
c2 <- (n - 1) * (p - 1) * ftabel / (n - p + 1)
c2

ifelse(T2 > c2, "Tolak Ho", "Gagal Tolak H0")[1]

i = 1
# simultan confidence interval
t(C[i, ]) %*% xbar - sqrt(c2 * t(C[i, ]) %*% S %*% C[i, ] / n)
t(C[i, ]) %*% xbar + sqrt(c2 * t(C[i, ]) %*% S %*% C[i, ] / n)
# ada halotin positif maka ada pengaruhnya positif
# ada interkasi (karena selang tidak melewati 0)
# c02 tinggi dan rendah sama saja

# bonferoni (lebih pendek)
(ttabel <- qt(1 - (0.05 / (2 * p)), n - 1))
t(C[i, ]) %*% xbar - ttabel * sqrt(t(C[i, ]) %*% S %*% C[i, ] / n)
t(C[i, ]) %*% xbar + ttabel * sqrt(t(C[i, ]) %*% S %*% C[i, ] / n)

