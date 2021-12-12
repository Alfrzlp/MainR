library(tidyverse)
indeks = sample(1:nrow(iris), 130)
train = iris[indeks,]
test = iris[-indeks,]

# lda-----------------------------------------------------------
library(MASS)
fit <- lda(Species~., data = train)
fit
#confussion matrix
hasil = fit %>% predict(test[,-5])
hasil

ct <- table(hasil$class, test[,5])
ct
# percent correct for each category
diag(prop.table(ct, 1)) # 1 artinya total prop setiap baris = 1
# akurasi
# diag = ambil diagonal saja (yang benar saja)
diag(prop.table(ct)) %>% sum() 


# knn-----------------------------------------------------------
library(class)
knnRes <- knn.cv(train[,1:4], train[,5],  k = 4)
table(knnRes, train[,5])


library(kknn)
modelknn = train.kknn(Species~., data = train, kmax = 9)
modelknn
# melihat miss klasifikasi
modelknn$MISCLASS

hasil = modelknn %>% predict(test[,-5])
tab = table(hasil, test[,5])
tab

# akurasi
sum(diag(tab))/sum(tab)
