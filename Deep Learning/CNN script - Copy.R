#Contoh deep learning menggunakan keras dan tensorflow


library(keras)
library(tensorflow)
library(EBImage)
search()
#set path
setwd("E://test/")
save_in<-("E://hasiltest/")
#Load images names
gambar<-list.files()
#set width
w<-100
#set height
h<-100

#Main loop resize images
for (i in 1:length(gambar))
# Try-catch is necessary since some images 
{result<-tryCatch({
  #image name
  imgname<-gambar[i]
  #Read image
  img<-readImage(imgname)
  #resize image to 100x100
  img_resized<-resize(img,w=w,h=h)
  path<-paste(save_in,imgname,sep=" ")
  #save image
  writeImage(img_resized, path, quality=70)
  #print status
  print(paste("done",i,sep=" "))
},
error=function(e){print(e)}
)}
 
#set path 
setwd("E://hasiltest")
gambar2<-list.files()
gambar2

gambar2<-lapply(gambar2,readImage)
str(gambar2)

display(gambar2[[4]])
dim(gambar2[[4]])

#crate train and test data
train<-gambar2[c(1:80,101:180)]
test<-gambar2[c(81:100,181:200)]

train[[5]]
display(train[[5]])
write.csv(train[[5]],"E://test/datatrain.csv")

par(mfrow=c(2,4))
for (i in 1:160) plot(train[[i]])

#resize
for (i in 1:160) {train[[i]]<-resize(train[[i]],32,32)}
for (i in 1:40) {test[[i]]<-resize(test[[i]],32,32)}
str(train)
str(test)

#combine
train<-combine(train)
x<-tile(train,20)
display(x,title='gambar')
dim(train)

test<-combine(test)
y<-tile(test,10)
display(y,title='gambar2')
dim(test)
dim(train)

#Reorder dimension
train<-aperm(train,c(4,1,2,3))
test<-aperm(test,c(4,1,2,3))
dim(train)
dim(test)

#Response
trainy<-c(rep(0,80),rep(1,80))
testy<-c(rep(0,20),rep(1,20))
trainy
testy

#On hot encoding
trainLabels<-to_categorical(trainy)
testLabels<-to_categorical(testy)
trainLabels
testLabels


#membuat model

model<-keras_model_sequential()
model%>%
  layer_conv_2d(filters=32,
                kernel_size=c(3,3),
                activation='relu',
                input_shape=c(32,32,3))%>%
  layer_conv_2d(filters=32,
                kernel_size=c(3,3),
                activation='relu')%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_dropout(rate=0.01)%>%
  layer_conv_2d(filters=64,
                kernel_size=c(3,3),
                activation='relu')%>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_dropout(rate=0.01)%>%
  layer_flatten()%>%
  layer_dense(units=256,activation='relu')%>%
  layer_dropout(rate=0.01)%>%
  layer_dense(units=2,activation='softmax')%>%
  compile(loss='categorical_crossentropy',
          optimizer= optimizer_sgd(lr=0.01,
                                   decay=1e-06,
                                   momentum=0.9,
                                   nesterov=T),
          metrics=c('accuracy'))
summary(model)

proses<-model%>%
  fit(train,
      trainLabels,
      epoch=30,
      batch_size=32,
      validation_split=0.2)

plot(proses)

# Evaluation & Prediction - train data 
model %>% evaluate(train, trainLabels) 
pred <- model %>% predict_classes (train) 
table(Predicted = pred, Actual = trainy) 

prob <- model %>% predict_proba(train) 
cbind(prob, Predicted_class = pred, Actual = trainy) 

# Evaluation & Prediction - test data 

model %>% evaluate(test, testLabels) 
pred <- model %>% predict_classes(test) 
table(Predicted = pred, Actual = testy) 

prob <- model %>% predict_proba(test) 
cbind(prob, Predicted_class = pred, Actual = testy) 


#save model 
save_model_hdf5(model,filepath='E://hasiltest.hdf5') 

#test Model 
setwd('E:/uji/') 

model=load_model_hdf5(filepath='E://hasiltest.hdf5') 
images <- list.files() 
images 
summary(images) 

list_of_images = lapply(images, readImage) 
head(list_of_images) 
display(list_of_images[[3]]) 

# Get the image as a matrix 
test <- list_of_images 

for (i in 1:20) {test[[i]] <- resize(test[[i]], 32, 32)} 
for (i in 1:20) {test[[i]] <- toRGB(test[[i]])} 

fixx <- combine(test) 
y <- tile(fixx, 5) 
display(y, title = 'Pics') 

str(fixx) 
Uji <- aperm(fixx, c(4, 1, 2, 3)) 
str(Uji) 

testy <- c(rep(0,10), rep(1,10)) 

# One hot encoding 
testLabels <- to_categorical(testy) 

pred <- model %>% predict_classes(Uji)
table(Predicted = pred, Actual = testy) 

model %>% evaluate(Uji, testLabels) 

prob <- model %>% predict_proba(Uji)
prob 
colnames(prob)<- c('Mobil','Motor') 
Duhan<-cbind(prob, Predicted_class = pred, Actual = testy) 
Duhan







