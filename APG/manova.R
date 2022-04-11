iris

# Uji Kesamaan Varians ----------------------------------------------------
# Ho : Matriks varians covarians sama
library(asbio)
Kullback(iris[, -5], iris$Species)

library(heplots)
boxM(iris[, -5], iris$Species)


# Outlier -----------------------------------------------------------------
library(rstatix)
iris %>% 
  mahalanobis_distance()



# Normalitas --------------------------------------------------------------
library(MVN)
mvn(
  iris[-5]
)



# Mnova -------------------------------------------------------------------
m <- manova(cbind(Sepal.Length, Sepal.Width) ~ Species, iris)
summary(m)

