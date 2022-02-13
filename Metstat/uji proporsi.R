x <- c(45, 55, 70)
y <- c(905, 890, 870)

prop.test(x, y + x, conf.level = 1 - 0.025)




setuju <- c(82, 70, 62)
menentang <- c(93, 62, 67)
takpendapat <- c(25, 18, 21)

tb <- as.table(rbind(setuju, menentang, takpendapat))
hasil <- chisq.test(tb)
hasil
hasil$expected


x <- c(45, 55, 70)
y <- c(905, 890, 870)

m <- as.table(rbind(x, y))
dimnames(m) <- list(
  keadaan = c("cacat", "tidak cacat"),
  giliran = c("pagi", "sore", "malam")
)
m
hasil <- chisq.test(m)
hasil


prop.test(m)


hasil$expected # e ij
hasil$observed # data
hasil$p.value

e <- matrix(hasil$expected, 2, 3) %>% round(., 1)
e
(((matrix(m, 2, 3) - e)^2) / e) %>% sum()



x <- c(21, 36, 30)
y <- c(48, 26, 19)

m <- as.table(rbind(x, y))
h <- chisq.test(m)
