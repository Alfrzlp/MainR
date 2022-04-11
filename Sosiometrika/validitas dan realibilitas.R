uji_Bartlett <- function(x) {
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x))
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1 - n + (2 * p + 5) / 6) * log(det(cor(x)))
  df <- p * (p - 1) / 2
  p.value <- pchisq(chisq, df, lower.tail = FALSE)
  names(chisq) <- "Khi-squared"
  names(df) <- "df"
  return(structure(list(
    statistic = chisq, parameter = df, p.value = p.value,
    method = method, data.name = data.name
  ), class = "htest"))
}



s <- "1 3 3 3 3 3 4 3 4 4 3
2 3 3 3 3 3 4 3 3 3 4
3 3 2 1 2 3 3 1 2 2 2
4 4 3 3 3 4 3 4 3 4 3
5 3 4 4 4 3 3 3 3 4 3
6 4 4 3 4 4 4 4 3 2 3
7 2 3 4 4 3 4 3 4 3 2
8 3 4 3 2 1 1 2 2 2 1
9 4 4 1 2 1 3 3 4 2 4
10 3 3 4 4 4 4 3 4 3 3
11 3 3 4 3 4 4 3 4 4 4
12 2 3 2 1 1 2 1 3 2 3"


dat <- dm::read_string(s) %>%
  dplyr::select(-1)
# r=0,576
total <- rowSums(dat)


# uji validitas -----------------------------------------------------------
library(psych)
library(GPArotation)

res <- dat %>% apply(2, function(x) cor(x, total))
ifelse(res < 0.576, "Tidak Valid", "Valid")

KMO(dat)
uji_Bartlett(dat)


# Uji Realibilotas --------------------------------------------------------
library(ltm)
cronbach.alpha(dat, CI = T)


var_dat <- dat %>% 
  cbind(total = rowSums(.)) %>% 
  apply(2, function(x) var(x))

var_dat
k <- ncol(dat)
sum(var_dat[-length(var_dat)])
# alpha
k*(1 - (sum(var_dat[-length(var_dat)])/var_dat[length(var_dat)]))/(k - 1)

