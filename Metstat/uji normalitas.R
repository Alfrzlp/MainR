# shapiro wilk
str <- "58 23 58 56 46 37 18 32
36 19 34 33 41 36 55 30
24 36 33 26 40 35 48 27"

df <- str.to.df(str, dim = c(24, 1))
df

colnames(df) <- "usia"
df

shapiro.test(df %>% pull())




# kolmogorov smirnov
str2 <- "58 78 84 90 97 70 90 86 82
59 90 70 74 83 90 76 88 84
68 93 70 94 70 110 67 68 75
80 68 82 104 92 112 84 98 80"

df2 <- str.to.df(str2, dim = c(36, 1))
colnames(df2) <- "berat"
df2

ks.test(df2$berat, "pnorm", 85, 15)





# liliefors
x <- c(7, 9, 11, 12, 14, 15, 11, 11, 11, 7, 7, 7)
library(nortest)
lillie.test(x)

lillie <- function(vektor, m = mean(vektor), std = sd(vektor)) {
  data <- data.frame(vektor) %>%
    rename(x = vektor) %>%
    group_by(x) %>%
    count() %>%
    ungroup() %>%
    mutate(
      fk = cumsum(n),
      `S(x)` = fk / sum(n),
      z = (x - m) / std,
      `F(x)` = pnorm(z),
      `|F(x)-S(x)|` = abs(`F(x)` - `S(x)`)
    ) %>%
    as.data.frame()
  print(max(data$`|F(x)-S(x)|`))
  print(paste("mean", mean(vektor)))
  print(paste("var", var(vektor)))
  return(data)
}
lillie(x)

lillie(c(6, 7, 8, 10, 12, 15))
lillie.test(c(6, 8, 8, 12, 14))


# jarque bera
library(moments)
skewness(x)
kurtosis(x)
jb <- n * (skewness(pull(df))^2 / 6 + (kurtosis(pull(df)) - 3)^2 / 24)
jb
normtest::jb.norm.test(x)
library(tsoutliers)
JarqueBera.test(df)


# goodnes of fit
str3 <- "9 11.4
24 25.8
51 51.6
66 71.2
72 67.8
48 44.5
21 20.2
9 7.5"
df3 <- str.to.df(str3, dim = c(8, 2))
colnames(df3) <- c("o", "e")
df3

df3 %>%
  mutate(x = ((o - e)^2) / e) %>%
  summarise(`x^2` = sum(x))










str <- "150 158 9
159 167 24
168 176 51
177 185 66
186 194 72
195 203 48
204 212 21
213 221 6
222 230 3"
df <- str.to.df(str, dim = c(9, 3))
colnames(df) <- c("bawah", "atas", "Oi")
df

df <- df %>%
  type_convert()
df

mean <- mean(rep(df$nilai_tengah, df$Oi))
sd <- sd(rep(df$nilai_tengah, df$Oi))
mean <- 184.3
sd <- 14.563

df <- df %>%
  mutate(
    p = pnorm((atas + 0.5 - mean) / sd) - pnorm((bawah - 0.5 - mean) / sd),
    p = round(p, 3),
    ei = sum(Oi) * p
  )
df

df <- rbind(df[1:7, ], df[8, ] + df[9, ]) %>%
  mutate(x = (Oi - ei)^2 / ei)
df

write.csv2(df, "D:/tugas/mt3.csv")

#    k-1-p = 10-1-2
qchisq(0.05, 7, lower.tail = F) # lower.tail false = luas kurva kanan

# n = 309
# mean = 184.41
# var = 233.72
# sd = 15.29
# X^2 obs = 7.178649
# X^2 tabel = 14.06714

x <- c(4, 7, 10, 4, 8, 12, 9)

-2.356 + 1.245 + log((0.93958 - 0.4533) / (1 - 0.93958))

-4.567 + 1.724 + log((0.951 - 0.2727) / (1 - 0.951))
pnorm(-0.2152306)

str <- "5,8 7,3 8,9 7,1 8,6 6,4 7,2 5,2
10,1 8,6 9,0 9,3 6,4 7,1 9,9 6,8"
no2 <- str.to.df(str, koma = T, dim = c(16, 1))

colnames(no2) <- "waktu"
no2 <- pull(no2)

sum((x - mean(x))^4) / length(x)

no2 %>%
  arrange(waktu)
shapiro.test(pull(no2))
lillie(no2 %>% pull())
lillie.test(no2 %>% pull())


(0.0551341^2 / 6 + (1.867603 - 3)^2 / 24) * 16



df <- data.frame(
  umur = c(7, 8, 8, 11, 12, 15, 6, 8, 8, 12, 14, 6, 7, 8, 10, 12, 15),
  pos = rep(c("melati", "mawar", "dahlia"), c(6, 5, 6))
)

bartlett.test(umur ~ pos, data = df)

x <- c(64, 83, 138)
y <- c(84, 67, 64)
tab <- as.table(rbind(x, y))
tab

h <- stats::chisq.test(tab)
h
h$expected
