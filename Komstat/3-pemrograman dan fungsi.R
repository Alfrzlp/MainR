#for loop
for(i in colors()[1:10]){
  print(i)
}

#while loop
p = TRUE
n = 1
while(p==TRUE){
  print(paste("test" ,n))
  n = n+1
  if(n==10){
    p = FALSE
  }
}

#repeat loop
n = 0
repeat{
  print('a')
  n = n + 1
  if(n==10){
    break
  }
}

#apply________________________________________________________________
#Margin 1=baris, 2=kolom
x = cbind(x1 = 3, x2 = c(1:4, 2:5))
#cari mean tiap baris
apply(x, MARGIN=1, FUN=mean )

apply(x, MARGIN=1, 
      FUN=function(x){
           max(x)-min(x)
} )

#lapply = list________________________________________________________
# output list
x = list(a=1:10, beta=exp(-3:3), logic=c(TRUE, FALSE, FALSE, TRUE) )
B = lapply(x, FUN = mean)
class(B)
#sapply = vektor, dataframe, list_____________________________________
# simplify true = vektor(default)
A = sapply(x, FUN=mean, simplify = TRUE)
class(A)

sapply(mtcars, FUN = summary)

#vapply______________________________________________________________
#lebih cepat, bisa menentukan output
vapply(x, fivenum, 
       c(min=0, "hinges bawah"=0, median=0, "hinges atas"=0, max=0))


#decision making-----------------------------------------------------
rnorm(2, mean=5, sd=12)
`
a = runif(1, 1,10)
a = format(round(a, 0), nsmall=0)`
tebak = readline(prompt="Masukkan tebakan angka  :")

a = as.numeric(a)

while(tebak != a){
  tebak = as.numeric(readline(prompt="Masukkan tebakan angka  :"))
  
  if(tebak > a){
    print("angka terlalu besar")
  }else if(tebak < a){
    print("angka terlalu kecil")
  }else{
    print("benar")
  }
}