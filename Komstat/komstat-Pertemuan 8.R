
#if statement
if(1==0){
  print(1)
}else{
  print(2)
}

w = 3
if(w<5){
   d=2
}else{
  d=10
}
d

#ifelse statement
x <- 1:10
ifelse(x<5 | x>8, x, 0)
ifelse(x>5, "high", "low")

#switch
x <- switch(3,
  "firs",
  "second",
  "third",
  "fourth"
)
x

inpt = "mean"
x <- 1:10
switch(inpt,
       mean = mean(x),
       median = median(x))

#Loop
#For-Loop
h <- seq(1, 10)
s <- c()
for(i in 1:10){
  #lakukan iterasi sebanyak 10x
  #memasukkan nilai hasilke objek s
  s[i] <- h[i]*10
}
s

sqr <- seq(1, 10, by=2)
sqr

res <- NULL #definisikan vektor hasil
resMat <- matrix(NA, 5, 2)

for(i in 1:5){
  res[i] <- sqr[i]^2
  resMat[i,] <- c(i, sqr[i]^2)
}
resMat
res

x <- 1:20
y <- NULL

for(i in seq(along=x)){
  if(x[i]<10){
    y <- c(y, x[i]-1)
  }else{
    stop("nilai x harus lebih kecil dari 10")
  }
}
y

#Data gini Ratio
Tahun <- rep(c(2009, 2010, 2011, 2012, 2013), times=4)
Gini <- c(.36, .36, .44, .42, .433, .36, .36, .41, .41, .411, .32,
          .34, .38, .38, .387, .38, .41, .4, .43, .439)
Prov <- rep(c("DKI Jakarta", "Jawa Barat", "Jawa Tengah", "DI Yogyakarta"), each=5)
Prov <- factor(Prov)
DataGini <- data.frame(Prov, Tahun, Gini)
head(DataGini)

#mendapatkan rentang nilai untuk sumbu x dan y
xrange <- range(DataGini$Tahun)
yrange <- range(DataGini$Gini)

#membuat plot ksosong
plot(xrange, yrange, type="n", xlab="Tahun", 
     ylab="Gini Ratio")

Prov <- unique(Prov)
Prov

for(i in 1:length(Prov)){
  datai <- DataGini[DataGini$Prov == Prov[i],]
  lines(datai$Tahun, datai$Gini, col=i+1, lty=i, lwd=2, pch=15+i)
  points(datai$Tahun, datai$Gini, col=i+1, pch=15+i)
}
legend("bottomright", legend = Prov, col=2:6, lty=1:5, lwd=2, pch=15:21)

#while
z <- 0
while (z<10) {
  #print selama z < 10
  z <- z+2
  print(z)
}

#repeat
sum <- 1
repeat{
  sum <- sum +2
  print(sum)
  if(sum > 11) 
    break
}

#apply
# 1 baris, 2 kolom, c(1,,2) keduannya

data("cars") #menggunakan data yang telah ada di r
head(cars)

#apply dengan fungsi yang telah ada
apply(cars, 2, mean)
apply(cars, 2, sd)

#menggunakan fungsi yang kita buat sendiri
x <- 1:15
test <- function(x){
  if(x<10)
    x-1
  else
    x/x
}

apply(as.matrix(x), 1, test)

#tapply
#untuk vektor atau array berjenis kategorik
data("iris")
head(iris)

#menghitung rata-rata petal.width kolom ke 4 pada setiap spesies
tapply(as.vector(iris[,4]), factor(iris[,5]), mean)
  
aggregate(iris[,1:4], list(iris$Species), mean)

#lapply dan sapply
#lapply menghasilkan list
#sapply menghasilkan vektor atau matriks
mylist <- as.list(cars[1:5,]) ##membuat list
mylist

#menghitung jumlah pada setiap elemen di list mylist
#dan hasilnya berupa list
lapply(mylist, sum)

#menghitung jumlah pada setiap elemen di list mylist
#dan hasilnya berupa vektor
sapply(mylist, sum)


#membuat fungsi dalam r

#fungsi pangkat
pangkat <- function(x, pow) x^pow
pangkat(2,5)

#fungsi jumlah nilai
intsum <- function(from, to){
  sum <- 0
  for(i in from:to)
    sum <- sum +i
  return(sum)
}
intsum(from=3, to=10)
intsum(3, 10)

#fungsi menampilkan deret fibonaci
fibo <- function(n){
  x <- c(0,1)
  while (length(x)<n) {
    position <- length(x)
    new <- x[position] + x[position-1]
    x <- c(x, new)
  }
  return(x)
}
fibo(10)

#fungsi dengan switch
exp1 <- function(x, type){
  switch (type,
    kuadrat = x^2,
    akar = sqrt(x),
    exp = exp(x)
  )
}

x <- 1:5
exp1(x, "kuadrat")
exp1(x, "akar")

#fungsi membuat tabel perkalian
kali <- function(N){
  mat = matrix(nrow=N, ncol=N)
  for(i in 1:N){
    for(j in 1:N){
      mat[i,j] = i*j
    }
    cat(i)
  }
  return(mat)
}

hasil = kali(6)
hasil

#grafik matematika
x <- seq(-2, 2, by=0.1)
y <- seq(-2, 2, by=0.1)
f2 <- function(x,y){x^2 + y^2}
z <- outer(x,y, f2)
persp(x,y,z, phi=30, col=2)

x <- seq(-10, 10, length=30)
y <- x
f <- function(x, y){
  r <- sqrt(x^2+y^2)
  return(10*sin(r)/r)
}

z <- outer(x,y, f)
z[is.na(z)] <- 1
persp(x,y,z, col="red")

#Berkerja dengan Tipe data String
month.name

#mengabung string/karakter
paste(month.name[1], month.name[2], sep = " Kemudian ")

id <- paste("ID", "Pelanggan", sep="_")
id

strsplit(id, "_")

#nama bulan dengan huruf depan A
grep("A", month.name)

month.name[grep("Ap", month.name)]
