library(spgwr)

data(columbus, package = "spData")
?columbus
glimpse(columbus)

columbus <- columbus %>%
  janitor::clean_names()

m1 <- lm(crime ~ inc + hoval, columbus)
summary(m1)


# GWR  --------------------------------------------------------------------

# mencari nilai bandwith optimum
h <- gwr.sel(crime ~ inc + hoval,
  coords = cbind(columbus$x, columbus$y),
  columbus, adapt = F,
  gweight = gwr.Gauss
)

mgwr <- gwr(crime ~ inc + hoval,
  coords = cbind(columbus$x, columbus$y),
  columbus, bandwidth = h,
  hatmatrix = T,
  gweight = gwr.Gauss
)
mgwr
mgwr$SDF

# uji kecocokan model
# Tolak Ho : Model Spasial lebih baik
BFC02.gwr.test(mgwr)

# uji pengaruh geografis terhadap setiap prediktor
LMZ.F3GWR.test(mgwr)

mgwr$bandwidth

mgwr$SDF[, 2:4]
mgwr$SDF[, c(2:4, 9, 11)]





data(georgia)
g.adapt.gauss <- gwr.sel(PctBach ~ TotPop90 + PctRural + PctEld + PctFB +
  PctPov + PctBlack, data = gSRDF, adapt = TRUE)
res.adpt <- gwr(PctBach ~ TotPop90 + PctRural + PctEld + PctFB + PctPov +
  PctBlack, data = gSRDF, adapt = g.adapt.gauss)
res.adpt


pairs(as(res.adpt$SDF, "data.frame")[, 2:8], pch = ".")
brks <- c(-0.25, 0, 0.01, 0.025, 0.075)
cols <- grey(5:2 / 6)
plot(res.adpt$SDF, col = cols[findInterval(res.adpt$SDF$PctBlack, brks,
  all.inside = TRUE
)])



# Mixed GWR ---------------------------------------------------------------
