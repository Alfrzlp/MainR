# Vektor
v <- 1:10
v
v <- c(1, 5, 3)
v
vc1 <- seq(from = 1, to = 100, by = 10)
vc1

# logical vektor
logical_vektor <- c(F, T, FALSE)
logical_vektor

# Vektor String
s <- c("jakarta", "surabaya", "bali")
s

# operasi Vektor
vc2 <- seq(1, 100, 10)
length(vc2)
sum(vc2)

# operasi Aritmatika pd Vektor
x <- 100:110
x
x * 2
x / 2
x - 1

x <- c(10, 20, 30)
y <- c(2, 2, 2)

# elemen 1 vektor 1 dikali elemen 1 vektor 2
x * y
x / y
# perkalian vektor
x %*% y

# subset index vektor
# elemen ke 1 dan 3
x[c(1, 3)]
# selain elemen ke 2
x[-2]

# faktor
# default jumlah level = jumlah angka
factor(1:3)
factor(1:3, levels = 1:5)

# memberi nama level
factor(1:3, levels = 1:5, labels = c("a", "b", "c", "d", "e"))

type <- rep(c("high", "medium", "low"), times = 10)
type <- factor(type)
type

grade <- rep(c("grade1", "grade2", "grade3", "grade4"), each = 5)
grade <- factor(grade)
grade

# matriks
mat <- matrix(c(2, 3, 1, 5, 4, 5, 6, 7, 2, 3, 1, 5, 4, 5, 6, 7), nrow = 4, ncol = 4)
mat

mat2 <- matrix(1:20, nrow = 4, ncol = 4)
mat2
dim(mat)

# operasi Matriks
# perkalian matriks
A <- matrix(c(1, 3, 2, 4, 8, 7), ncol = 3)
A
8 * A

# penjumlahan
B <- matrix(c(5, 8, 3, 4, 2, 7), ncol = 3, byrow = T)
B
A + B

A1 <- matrix(c(1, 3, 2, 2, 8, 9), ncol = 2)
B1 <- matrix(c(5, 8, 4, 2), ncol = 2)

A1
B1

A1 %*% B1 # dot product
t(A1) # Transpose
diag(A1) # diagonal matriks
det(B1) # determinan
solve(B1) # Invers matriks, harus bujur sangkar

# Data Frame
data1 <- data.frame(ID = 1:20, grade, sex = rep(c("male", "female"), each = 10))
data1

head(data1) # 6 data awal
tail(data1) # 6 data akhir

dim(data1)

# binding
A1
B1
A
D <- cbind(A, B1) # column binding
D
E <- rbind(A1, B1) # row binding
E

# list
mylist <- list(A1, B1, 5, "flanders", grade)
mylist
