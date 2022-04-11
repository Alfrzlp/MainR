
# Univariate QQplot -------------------------------------------------------
x <- c(-1, -.1, .16, .41, .62, .8, 1.26, 1.54, 1.71, 2.3)

dat <-
  data.frame(x, j = 1:length(x)) %>%
  mutate(
    jj = (j - 0.5) / nrow(.),
    qj = qnorm(jj)
  )

dat %>%
  ggplot(aes(x = qj, y = x)) +
  geom_point()

nrow(dat)
(rq <- cor(dat$x, dat$qj))
# karena nilai rq > 0.9351 maka gagal tolak H0 (Normal)


qqnorm(x, pch = 1, frame = FALSE)
qqline(x, col = "steelblue", lwd = 2)




imunisasi %>% 
  pivot_longer(cols = -1, names_to = 'jenis', values_to = 'persentase') %>% 
  ggplot(aes(sample = persentase, color = jenis)) +
  stat_qq(show.legend = F) +
  stat_qq_line(show.legend = F) +
  facet_wrap(vars(jenis), scales = 'free')


# Multivariate QQ Plot ----------------------------------------------------
x1 <- matrix(c(126.974, 96.933, 86.656, 63.438, 55.264, 50.976, 39.069, 36.156, 35.209, 32.416))
x2 <- matrix(c(4.224, 3.835, 3.51, 3.758, 3.939, 1.809, 2.946, 0.359, 2.48, 2.413))

nilai_chisq <- qchisq(0.5, 2, lower.tail = F)
nilai_chisq

dat <-
  data.frame(x1, x2) %>%
  mutate(
    d2 = mahalanobis(., center = colMeans(.), cov = cov(.))
  ) %>%
  arrange(d2) %>%
  mutate(
    j = 1:nrow(.),
    jj = (j - 0.5) / nrow(.),
    # p = 2
    qcp = qchisq(1 - jj, 2, lower.tail = F),
    kondisi = ifelse(d2 >= nilai_chisq, 'luar kontur', 'dalam kontur')
  )
dat

# lebih dari 50% diluar kontur maka tidak normal
table(dat$kondisi)



dat %>%
  ggplot(aes(x = qcp, y = d2, sample = d2)) +
  labs(
    y = expression(paste("mahalanobis distance ( ", d^{2}, ")"))
  ) +
  geom_point() 



dat %>%
  ggplot(aes(sample = d2)) +
  stat_qq() +
  stat_qq_line() 


# tidak manual ------------------------------------------------------------
library(RVAideMemoire)
X <- cbind(x1, x2)
mqqnorm(X)

d2 <- mahalanobis(X, center = colMeans(X), cov = cov(X))
qqnorm(d2, pch = 1)
qqline(d2, col = "steelblue", lwd = 2)


styler::style_file(rstudioapi::getSourceEditorContext()$path)



# mahalanobis distance ----------------------------------------------------
X

d <- sweep(X, 2L, colMeans(X))
rowSums(d %*% solve(var(X)) * d)

# cari satu2
t(d[1,]) %*% solve(var(X)) %*% d[1,]
apply(d, 1, function(d) (t(d) %*% solve(var(X)) %*% d))
