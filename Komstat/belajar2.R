# function
tambah <- function(a, b){
  c = a+b
  d = a-b
  out = list(c, d)
  return (out)
}

hasil = tambah(2,3)
print(hasil[2])  #R mulai dari 1
#
x = c(2,3,1,7,8,9)
y = c(1,5,9,2,3,8)
print(x+y)

#membentuk matriks
c = matrix(c(2,3,4,5,6,7), nrow = 3, ncol = 2)
print(c)
#membentuk matriks urut kekanan
c2 = matrix(c(2,3,4,5,6,7), nrow = 2, ncol = 3, byrow = TRUE)
print(c2)
print(c*c)

#dataframe
dt = Orange
print(dt)
print(dt[35, 2]) #baris , kolom
