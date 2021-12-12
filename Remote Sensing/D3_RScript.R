#set working directory
#load library
library(raster)
rasterOptions(progress = "Text", tmpdir = "D:/Datasets/Data")

#persiapan data
s2.10m <- stack("D:/Datasets/Data/Medium Resolution/s2_l1c_10m_crop.tif")
s2.20m <- stack("D:/Datasets/Data/Medium Resolution/s2_l1c_20m_crop.tif")
s1 <- stack(list.files(getwd(), "GRDH_1SDV"))
s1 <- stack("D:/Datasets/Data/Medium Resolution/S1B_IW_GRDH_1SDV_20190707T215230_crop.tif")

#cek data
s2.10m
s2.20m
s1

#reproject sentinel 1
crs(s1)
crs(s2.10m)
crs(s2.20m)
s1.proj <- projectRaster(s1, crs = crs(s2.10m))
crs(s1.proj)

#resample s2.10m dan s1 ke resolusi 20 m s2.20m
s1.res <- resample(s1.proj, s2.20m, "bilinear")
s2.10res <- resample(s2.10m, s2.20m, "bilinear")

#stack input data
names(s1.res) <- c("vv", "vh")
names(s2.10res) <- c("b2", "b3", "b4", "b8")
names(s2.20m) <- c("b5", "b6", "b7", "b8a", "b11", "b12")
all.stack <- stack(s1.res, s2.10res, s2.20m)
plotRGB(all.stack, 5, 4, 3, scale = 9050, stretch = "lin")

#load roi dan mengubah roi menjadi data frame untuk input algoritma
roi <- raster("C:/Users/Ridson Alfarizal/Documents/Main R/Remote Sensing/roi_utm.tif")
roi.res <- resample(roi, all.stack, "ngb")

#plot data
plot(roi.res)

#masking all.stack dan stack dengan ROI
mask <- roi.res
mask[!is.na(mask)] <- 1
plot(mask)
masked.stack <- all.stack * mask
stack.roi <- stack(roi.res, masked.stack)
plot(stack.roi)
plotRGB(stack.roi, 6, 5, 4, scale = 9050, stretch = "lin")
names(stack.roi) <- c("lc", names(s1.res), names(s2.10res), names(s2.20m))

#mengubah rasterstack menjadi data frame
dframe <- values(stack.roi)
dframe <- data.frame(dframe)
head(dframe)

#menghilangkan NA dari dataframe
dframe <- dframe[complete.cases(dframe),]
head(dframe)

#splitting training dan testing
library(rsample)
dframe_split <- initial_split(dframe, prop = .7)
train <- training(dframe_split)
test <- testing(dframe_split)

#running random forest
library(randomForest)
rf.model <- randomForest(as.factor(lc)~., data = train)

#check model dan important variable
rf.model
varImpPlot(rf.model)

#lakukan pada data test, lalu bandingkan akurasinya
library(caret)
rf.test <- predict(rf.model, test)
cm <- confusionMatrix(factor(rf.test), factor(test$lc))
cm

#aplikasikan pada data raster
pred.rf <- raster::predict(all.stack, rf.model, ext= extent(all.stack) , progress = "text", type = "class")
writeRaster(pred.rf, "classified_map.tif", format = "GTiff", datatype = "INT1U")
