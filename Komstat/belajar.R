# and &&, or ||
jumlah = 0

n = as.numeric(readline(prompt = 'Banyak data  :'))

for (i in 1:n){
  data = as.numeric(readline(prompt = 'data  :'))
  jumlah = data + jumlah
}
print(paste0('jumlah ',jumlah))

if (jumlah>50){
  print('jumlahnya besar')
}else
  if (jumlah < 50){
  print('jumlahnya kecil')
  }
print(paste('rata -rata :',jumlah/n))
