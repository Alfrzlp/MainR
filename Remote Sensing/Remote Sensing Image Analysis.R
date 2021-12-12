# https://biogeo.ucdavis.edu/data/rspatial/rsdata.zip

library(raster)
setwd(r'(D:\_Datasets\Remote sensing)')

# Blue
b2 <- raster('rs/LC08_044034_20170614_B2.tif')
# Green
b3 <- raster('rs/LC08_044034_20170614_B3.tif')
# Red
b4 <- raster('rs/LC08_044034_20170614_B4.tif')
# Near Infrared (NIR)
b5 <- raster('rs/LC08_044034_20170614_B5.tif')

b2
# information
crs(b2)
# Number of cells, rows, columns
ncell(b2)
dim(b2)
# spatial resolution
res(b2)
# Number of bands
nlayers(b2)
# Do the bands have the same extent,
# number of rows and columns, projection, resolution, and origin
compareRaster(b2, b3)

s <- stack(b5, b4, b3)
# Check the properties of the RasterStack
s


filename <- paste0('rs/LC08_044034_20170614_B',1:11,'.tif')
filename
l8 <- stack(filename)
l8

par(mfrow = c(2,2))
plot(b2, main = "Blue", col = gray(0:100 / 100))
plot(b3, main = "Green", col = gray(0:100 / 100))
plot(b4, main = "Red", col = gray(0:100 / 100))
plot(b5, main = "NIR", col = gray(0:100 / 100))


plotRGB(stack(b4, b3, b2),
        axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")
# "false color" image in which NIR, red, and green
# it makes it easy to see the vegetation (in red).
plotRGB(stack(b5, b4, b3), stretch = 'lin')
