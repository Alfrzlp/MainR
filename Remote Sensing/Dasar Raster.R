# sp is the central package supporting spatial data analysis in r
# sf terbarunya namun sp yang paling banyak dipakai
# SpatialPoints, SpatialLines, SpatialPolygons. These classes only represent geometries
# To also store attributes -> SpatialPolygonsDataFrame and SpatialPointsDataFrame

library(sp)
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5, -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9, 36.2, 39, 41.6, 36.9)
lonlat <- cbind(longitude, latitude)

pts <- SpatialPoints(lonlat)
class(pts)
showDefault(pts)


pts <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
showDefault(pts)

# raster package to improve how Spatial objects are printed.
library(raster)


# Point -------------------------------------------------------------------
precipvalue <- runif(nrow(lonlat), min = 0, max = 100)
df <- data.frame(ID = 1:nrow(lonlat), precip = precipvalue)
df

ptsdf <- SpatialPointsDataFrame(pts, data = df)
ptsdf
showDefault(ptsdf)

# Lines and Polygons ------------------------------------------------------
lon <- c(107.16770371007445, 107.78843124913695, 107.78843124913695, 107.16770371007445, 107.16770371007445)
lat <- c(-7.143541567965983, -7.143541567965983, -6.680017200258927, -6.680017200258927, -7.143541567965983)

lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
lonlat <- cbind(lon, lat)

lns <- spLines(lonlat, crs = CRS("+proj=longlat +datum=WGS84"))
lns

pols <- spPolygons(lonlat, crs = CRS("+proj=longlat +datum=WGS84"))
pols

plot(pols, col = "yellow")
plot(lns)

# A RasterLayer object represents single-layer (variable) raster data
r <- raster(ncol = 10, nrow = 10, xmx = -80, xmn = -150, ymn = 20, ymx = 60)
r
values(r) <- runif(ncell(r))
r
# bisa ditimpa valuenya
values(r) <- 1:ncell(r)
r

plot(r)


# RasterStack and RasterBrick ---------------------------------------------

# RasterBrick can only be linked to a single (multi-layer) file.
# In contrast, a RasterStack can be formed from separate files
# and/or from a few layers ('bands') from a single file
# Brick kadang lebih efisien dari stack
r2 <- r * r
r3 <- sqrt(r)
s <- stack(r, r2, r3)
s
plot(s)

# And you can make a RasterBrick from a RasterStack
b <- brick(s)
b
plot(b)

s <- shapefile("D:/_Datasets/gadm36_IDN_shp/gadm36_IDN_2.shp")
s


shapefile(jabar, "D:/_Datasets/Jabar/jabar.shp", overwrite = TRUE)

# writeRaster(s, 'output.tif', overwrite=TRUE)


# Sistem koordinat --------------------------------------------------------

# estimasi long lat butuh model bentuk bumi yang disebut dantum
# dantum sederhana seperti peta dasar
# yang terkenal WGS84
# my longitude/latitude relative to the WGS84 datum

# how to transform this three dimensional angular system to
# a two dimensional planar (sometimes called "Cartesian") system
# The different types of planar coordinate reference systems are
# referred to as 'projections'. Examples are 'Mercator', 'UTM',
# 'Robinson', 'Lambert', 'Sinusoidal' 'Robinson' and 'Albers'.
# tidak ada projections terbaik. semua sesuai kebutuhan

library(rgdal)
epsg <- make_EPSG()
i <- grep("indonesia", epsg$note, ignore.case = TRUE)
# first three
epsg[i[1:3], ]

crs(s)

# menambah crs
pp <- s
crs(pp) <- NA
crs(pp)
## CRS arguments: NA
crs(pp) <- CRS("+proj=longlat +datum=WGS84")
crs(pp)

# cara ini kurang baik, sama seperti melabeli sepeda dengan mobil
# tetap saja sepeda

# Transforming vector data
newcrs <- CRS("+proj=robin +datum=WGS84")
rob <- spTransform(s, newcrs)
rob


# vector data manipulation ------------------------------------------------
s
data.frame(s)
geom(s)

jabar <- s[s$NAME_1 == "Jawa Barat", ]
plot(jabar)

e <- extent(107, 107.2, -8, -4)
jabare <- crop(jabar, e)
plot(jabar)
plot(jabare, col = "light blue", add = TRUE)
plot(e, add = TRUE, lwd = 3, col = "red")


# Raster data manipulation ------------------------------------------------

x <- raster(
  ncol = 16, nrow = 32, xmn = 100, xmx = 1000,
  ymn = 100, ymx = 900
)
x
res(x)
res(x) <- 100
x
ncol(x)
nrow(x)
projection(x) <- "+proj=utm +zone=48 +datum=WGS84"
x

hasValues(x)
values(x) <- runif(ncell(x))
ncell(x) # kayaknya ncol * nrow


plot(x, main = "Raster with 72 cells")
dim(x)


r1 <- r2 <- r3 <- raster(nrow = 10, ncol = 10)
# Assign random cell values
values(r1) <- runif(ncell(r1))
values(r2) <- runif(ncell(r2))
values(r3) <- runif(ncell(r3))

s <- stack(r1, r2, r3)
s
inMemory(s)
b <- brick(r1, r2, r3)
b <- brick(s)
b
inMemory(b)
nlayers(s)

# deafult layar ke 1
raster(b, layer = 2)


# Maps --------------------------------------------------------------------
plot(b, add = T)

spplot(jabar)
jabar
