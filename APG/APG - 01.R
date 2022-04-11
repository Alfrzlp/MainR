library(readxl) 

# Data --------------------------------------------------------------------
imunisasi <- read_excel('D:/RKD2018_imunisasi.xlsx')
head(imunisasi)


# Matrix (n x p) ----------------------------------------------------------
Y <- unname(as.matrix(imunisasi[, -1]))
Y


# vektor rata-rata --------------------------------------------------------
colMeans(Y)

mat1 <- matrix(1, nrow = nrow(Y), ncol = nrow(Y))
xbar <- (t(mat1) %*% Y)/nrow(Y)
xbar



# matriks ragam-peragam ---------------------------------------------------
# yang berada pada diagonal merupakan varians,
# dan elemen matriks yang lain merupakan covarians
var(Y)
(S <- (t(deviation_matrix(Y)) %*% deviation_matrix(Y))/(nrow(Y) - 1))


# matriks korelasi --------------------------------------------------------
cor(Y)


V12 <- diag(sqrt(diag(S)))
V12
(R <- solve(V12) %*% S %*% solve(V12))





# Multivariate Normality -------------------------------------------------
dat <- 
  imunisasi %>% 
  dplyr::select(-prov) %>% 
  mutate(
    d2 = mahalanobis(., center = colMeans(.), cov = cov(.))
  ) %>%
  arrange(d2) %>%
  mutate(
    j = 1:nrow(.),
    jj = (j - 0.5) / nrow(.),
    qcp = qchisq(1 - jj, 5, lower.tail = F)
  )


dat
as.data.frame(dat)
qchisq(0.5, 5, lower.tail = F)







dat %>%
  ggplot(aes(sample = d2)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    y = expression(paste("mahalanobis distance ( ", d^{2}, ")"))
  ) +
  theme_bw()

library(RVAideMemoire)
mqqnorm(X)


imunisasi %>%  
  pivot_longer(-prov, names_to = 'jenis', values_to = 'persentase') %>% 
  ggplot(aes(sample = persentase, color = jenis)) +
  stat_qq(show.legend = F) +
  stat_qq_line(show.legend = F) +
  facet_wrap(~jenis, scales = 'free_y')


imunisasi %>% 
  dplyr::select(-prov) %>% 
  apply(2, 
    FUN = function(x){
      v <- qqnorm(x, plot.it = F)
      return(cor(v$x, v$y))
    }
  )
# 0.9682


library(MVN)
dt_imunisasi <- 
  imunisasi %>%  
  pivot_longer(-prov, names_to = 'jenis', values_to = 'persentase')

mvn(data = dt_imunisasi[-1], subset = "jenis")
result = mvn(data = dt_imunisasi[-1], subset = "jenis", 
             mvnTest = "hz", transform = T,
             univariateTest = "AD", univariatePlot = "qq",
             multivariatePlot = "qq", 
             showOutliers = TRUE, showNewData = TRUE)



#### Multivariate Normality Result
result$multivariateNormality
### Univariate Normality Result
result$univariateNormality
### Descriptives
result$Descriptives
### Multivariate Outliers
result$multivariateOutliers
### New data without multivariate outliers
result$newData







# transformasi ------------------------------------------------------------
library(MASS)
bc <- boxcox(lm(imunisasi$BCG~1))
(lambda <- bc$x[which.max(bc$y)])

bc <- boxcox(lm(imunisasi$campak ~ 1))
(lambda <- bc$x[which.max(bc$y)])






# sebelum transformasi
library(MVN)
mvn(
  X, 
  mvnTest = 'mardia',
  univariateTest = 'SW',
  multivariatePlot = 'qq',
  showOutliers = T
)

# transformasi
X_trans <- 
  imunisasi %>% 
  mutate_at(vars(BCG, campak), ~ .x ^ 2) %>% 
  dplyr::select(-prov) %>% 
  as.matrix()

mvn(
  X_trans, 
  mvnTest = 'mardia',
  univariateTest = 'SW',
  multivariatePlot = 'qq',
  multivariateOutlierMethod = 'adj',
  showOutliers = T
)

# buang jogja
mvn(
  X_trans[-14,], 
  mvnTest = 'mardia',
  univariateTest = 'SW',
  multivariatePlot = 'qq'
)
