df <- data.frame(state.x77)

# Membuat plot antara Illiteracy dengan HS Grad
plot(
  x = df$Illiteracy, y = df$HS.Grad, xlab = "Illiteracy",
  ylab = "High School Graduate", pch = 16, col = "red",
  main = "Scatter Plot", sub = "Illiteracy vs. High School Graduate"
)

# Mendefinisikan rentang axis
plot(
  x = df[, 3], y = df[, 6], xlab = "Illiteracy",
  ylab = "High School Graduate", pch = 16, col = 2,
  main = "Scatter Plot", sub = "Illiteracy vs. High School Graduate", xlim = c(0, 3), ylim = c(0, 100)
)

# Menambah garis regresi
abline(lm(df[, 6] ~ df[, 3]), lwd = 2, col = "blue")

# membagi tempat plot 1x2
par(mfrow = c(1, 2))

# Histogram frekuensi
df <- data.frame(df)
hist(df$Income, xlab = "Income", main = "Histogram of Income")

# Histogram dengan Density Plot
hist(df$Income,
  xlab = "Income", main = "Histogram of Income",
  col = "grey", probability = T
)
lines(density(df$Income), lwd = 2)

# Boxplot
par(mfrow = c(1, 1))
boxplot(df$Murder, xlab = "Murder Rate", ylab = "Rate")
boxplot(df[1:3], xlab = "variable")

# Membuat boxplot dari data yang digabung
A <- c(12.9, 13.5, 12.8, 13.6, 17.2, 13.2, 12.6, 15.3, 14.4, 11.3)
B <- c(14.7, 15.6, 15.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 13.1)

dt <- cbind(A, B)
boxplot(dt, xlab = "group", ylab = "waktu (detik)")

# Boxplot untuk beberapa grup
df$IncomeCode <- cut(df$Income,
  breaks = c(-Inf, 4000, 4800, Inf),
  labels = c("low", "medium", "high")
)

plot(df$IncomeCode, df$Illiteracy, xlab = "Income", ylab = "Illiteracy")

# Stem Leaf Plot
stem(x = df$Murder)

# Melakukan tabulasi
tab <- table(df$IncomeCode)
tab

# Diagram batang
barplot(height = tab, xlab = "Income", ylab = "Frequency")

data("chickwts")
barplot(height = table(chickwts$feed), xlab = "Feed", ylab = "Frequency", col = rainbow(6))

barplot(height = table(chickwts$feed), xlab = "Feed", ylab = "Frequency", density = c(10, 20, 30, 40, 50), border = "blue", col = 2)

tahun <- c("2009", "2010", "2011", "2012", "2013")
dki <- c(0.36, 0.36, 0.44, 0.42, 0.433)
jabar <- c(0.36, 0.36, 0.41, 0.41, 0.411)
jateng <- c(0.32, 0.34, 0.38, 0.38, 0.387)
yogya <- c(0.38, 0.41, 0.40, 0.43, 0.43)

# Menggabungkan variabel
dt1 <- cbind(dki, jabar, jateng, yogya)

# Membuat barplot
barplot(
  height = dt1, xlab = "Provinsi", ylab = "Gini Ratio",
  col = rainbow(5), beside = T, ylim = c(0, 0.5)
)
legend(x = "topleft", y = tahun, cex = 0.6, bty = "n", fill = rainbow(5))

barplot(
  height = t(dt1), xlab = "Tahun", col = rainbow(4),
  names.arg = tahun, ylab = "Gini Ratio", ylim = c(0, 2)
)
legend(x = "topleft", y = colnames(dt1), cex = 0.7, fill = rainbow(4))

komuter <- cbind(
  Komuter = c(285469, 386475, 127215, 338561, 154721),
  NonKomuter = c(1760765, 2226395, 714628, 1940492, 1302894)
)
rownames(komuter) <- c(
  "Jakarta Selatan", "Jakarta Timur",
  "Jakarta Pusat", "Jakarta Barat",
  "Jakarta Utara"
)
komuter

barplot(t(komuter), xlab = "Kota", ylab = "Jumlah Penduduk > 5 tahun", ylim = c(0, 4e6), col = 2:3)
legend("topleft", colnames(komuter), cex = 0.8, fill = 2:3)

tab2 <- prop.table(komuter, 1)
barplot(t(tab2), xlab = "Kota", ylab = "Persen", col = 2:3)
legend("topleft", colnames(komuter), cex = 0.8, fill = 2:3)

# Simple pie chart
pie(tab,
  col = rainbow(length(tab), start = 0.1, end = 0.8),
  clockwise = T
)
legend("topright",
  legend = row.names(tab), cex = 1.3,
  title = "Income", bty = "n", pch = 15,
  col = rainbow(length(tab), start = 0.1, end = 0.8),
  ncol = 1
)

# qq Plot
qqnorm(df$Income)
qqline(df$Income)

shapiro.test(df$Income)

# Scatter plot
pairs(df[, 2:6], col = 2, pch = 16)

M <- cor(df[, 2:6])
head(M)


library(corrplot)
corrplot(M, method = "circle")

corrplot(M, method = "ellipse")

tahun <- as.numeric(c("2009", "2010", "2011", "2012", "2013"))
dki <- c(0.36, 0.36, 0.44, 0.42, 0.433)
jabar <- c(0.36, 0.36, 0.41, 0.41, 0.411)

plot(tahun, dki,
  xlab = "Tahun", ylab = "Gini Ratio", type = "l",
  lwd = 2, pch = 15, ylim = c(0.3, 0.5), main = "Gini Ration DKI Jakarta"
)
text(tahun, dki, labels = as.character(dki))

# plot DKI
plot(tahun, dki,
  xlab = "Tahun", ylab = "Gini Ratio", type = "l",
  lwd = 2, pch = 15, ylim = c(0.3, 0.5)
)

# Plot jabar
lines(tahun, jabar, lty = 2, col = 2, lwd = 2)

# Tampilkan titik
points(tahun, dki, pch = 17, cex = 1.5)
points(tahun, jabar, pch = "+", cex = 1.5, col = 2)

legend("topleft", c("DKI", "Jabar"),
  col = 1:2, pch = list(17, 3),
  lty = 1:2, lwd = 2
)

plot(NA, xlim = c(0, 10), ylim = c(0, 10), xlab = "x", ylab = "y")

abline(0, 1, lwd = 2)
abline(h = 2, col = 2, lwd = 2, lty = 2)
abline(v = 5, col = 3, lwd = 2, lty = 3)
