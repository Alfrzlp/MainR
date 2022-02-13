.libPaths() # get library location
library()
# %% sisa bagi

# vektor sama dengan list, tapi list bisa menampung berbagai macam tipe data
# vektor__________________________________________________________
nilai <- c(20, 30, 40, 50, 60, 70, 80, 90)
# kecuali 1
nilai[-1]
# menghapus 1
nilai <- nilai[-1]
# tanpa indek ke 1 sampai 3
nilai[-1:-3]
# syarat harus >80
nilai[nilai > 80]
# syarat
nilai[nilai > 30 & nilai < 60]
# atau
nilai[nilai > 80 | nilai < 30]
nilai[nilai + 10 > 80]

# 3 smpai 8, jarak antar 0.5
y <- seq(3, 8, 0.5)
y
# 1, 2 diulang 6x
z <- rep(c(1, 2), 6)
z

x <- c(2, 3, 4, NA, 9)
# ffalse anf true
is.na(x)
x[4] <- 9 # masukin nilai
x
prod(x)
sd(x) # sd dan var sampel

y <- 1:5
y

x * y
x^y

log(y)

sort(nilai)
# selisih data2 - data1, data3-data2
diff(nilai)
max(nilai)
min(nilai)
pi

options(digits = 23)
pi

a <- c(10, 10, 20, 30, 40, 10, 60, 60, 60, 30, 40, 40, 80, 80, 90, 90)
# 10 ada berapa dll..
table(a)
# bisa juga melohat proporsi
table(a) / length(a)

factor(a)
plot(table(a))
# Matrik__________________________________________________________
mat <- matrix(c(1, 2, 3, 4, 5, 6),
  nrow = 2,
  dimnames = list(c("row1", "row2"), c("col1", "col2", "col3"))
)
mat
dim(mat) # dimensi matrik, bisa ncol dan nrow
mat[2, ]
mat[c(2, 1), ] # baris 2 dan 1
mat[2, 3] # baris 2 kolom 3
mat["row1", ] # berdasarkan nama

colSums(mat)
z <- rowSums(mat)
z[1]

rowMeans(mat)

mat2 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  nrow = 3, ncol = 3,
  dimnames = list(c("row1", "row2", "row3"), c("col1", "col2", "col3"))
)
mat2
col4 <- c(0, 0, 0)
m <- cbind(mat2, col4)
colnames(m)[4] <- "b"
m

apply(mat2, 1, median) # (data matriks, 1=baris/2=kolom, fungsi)

sampel <- c(1.01, 0.97, 1.03, 1.04, 0.99, 0.98, 0.99, 1.01, 1.03)
var(sampel)

srs <- function(N, n, nilai) {
  return(c(mean(nilai), var(nilai)))
}

srs(150, 30, sampel)
