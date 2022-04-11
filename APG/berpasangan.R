# input matrix xbar, var, C

repeated_md <- function(xbar, S, C, n, alpha = 0.05){
  p <- ncol(S)
  T2 <- n * t(C %*% xbar) %*% solve(C %*% S %*% t(C)) %*% (C %*% xbar)
  ftabel <- qf(alpha, p - 1, n - p + 1, lower.tail = F)
  c2 <- (n - 1) * (p - 1) * ftabel / (n - p + 1)
  
  ttabel <- qt(1 - (alpha / (2 * p)), n - 1)
  ci <- apply(C, 1, function(c){
    s.low <- t(c) %*% xbar - sqrt(c2 * t(c) %*% S %*% c / n)
    s.up <- t(c) %*% xbar + sqrt(c2 * t(c) %*% S %*% c / n)
    
    bon.low <- t(c) %*% xbar - ttabel * sqrt(t(c) %*% S %*% c / n)
    bon.up <- t(c) %*% xbar + ttabel * sqrt(t(c) %*% S %*% c / n)
    return(c(s.low, s.up, bon.low, bon.up))
  })
  ci <- `colnames<-`(t(ci), c('Lower', 'Upper', 'bonferroni Lower', 'bonferroni Upper'))
  
  return(list(
    T2 = T2,
    c2 = c2, 
    ftabel = ftabel,
    ttabel = ttabel,
    ci = ci
  ))
}




# input matrix x1, x2 dan n
paired_test <- function(x1, x2, alpha = 0.05){
  D <- x1 - x2
  n <- nrow(D)
  p <- ncol(D)
  dbar <- 1 / n * t(D) %*% rep(1, n)
  Sd <- var(D)
  
  T2 <- n * t(dbar) %*% solve(Sd) %*% dbar
  ftabel <- qf(alpha, p, n - p, lower.tail = F)
  c2 <- (n - 1) * p * ftabel / (n - p)
  ttabel <- qt(1 - alpha / (2 * p), n - 1)
  

  ci <- data.frame(
    lower = dbar - sqrt(c2 * diag(Sd) / n),
    upper = dbar + sqrt(c2 * diag(Sd) / n),
    bon.lower = dbar - ttabel * sqrt(diag(Sd) / n),
    bon.upper = dbar + ttabel * sqrt(diag(Sd) / n)
  )
  
  return(list(
    D,
    dbar = dbar,
    Sd = Sd,
    T2 = T2,
    ftabel = ftabel,
    c2 = c2,
    ttabel = ttabel,
    ci = ci
  ))
}




# Paired ------------------------------------------------------------------
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
  read.table(textConnection(s)) %>%
  `colnames<-`(rep(c("bod", "ss"), 2))

comlab <- dat[, 1:2]
statelab <- dat[, 3:4]



paired_test(comlab, statelab)

# Pengukuran Berulang -----------------------------------------------------
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
  read.table(textConnection(s1)) %>% 
  `colnames<-`(paste0('tr', 1:4))

n <- nrow(dat)
xbar <- 1 / n * t(dat) %*% rep(1, n)
S <- var(dat)
C <- matrix(c(-1, -1, 1, 1, 1, -1, 1, -1, 1, -1, -1, 1), nrow = 3, byrow = T)



repeated_md(xbar, S, C, n = 40)
