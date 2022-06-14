pak::pak('semPlot')

# analisis jalur dapat diselesaikan dengan
# - Pendekatan korelasi sederhana
# - Pendekatan regresi linear


# Data --------------------------------------------------------------------
dat <- 
  tribble(
    ~x1, ~x2, ~x3,
    30, 70, 6, 
    32, 78, 7,
    45, 56, 5, 
    24, 45, 5,
    46, 68, 6,
    32, 67, 7,
    33, 54, 6, 
    35, 50, 8,
    20, 45, 6,
    41, 70, 8
  )

head(dat)
glimpse(dat)
cor(dat)

# Model -------------------------------------------------------------------
mod.id = '
x2 ~ x1
x3 ~ x1 + x2
'


library(lavaan)
mod.est <- sem(model = mod.id, data = scale(dat))
mod.est <- cfa(model = mod.id, data = scale(dat))

summary(mod.est, rsquare = T)
summary(mod.est, fit.measures = TRUE)

fitmeasures(hz.fit, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))
# koefisien determinasi
0.44^2

# struktur 2
matrix(c(-0.02, 0.39), ncol = 2) %*% matrix(cor(dat)[1:2, 3])



# menguji perbedaan koefisien p31 dan p32




m1 <- lm(x2 ~ x1, data = as.data.frame(scale(dat)))
summary(m1)
m2 <- lm(x3 ~ x1 + x2, data = as.data.frame(scale(dat)))
summary(m)



# Path Diagram ------------------------------------------------------------
library(semPlot)

semPaths(
  object = mod.est,
  what = "path",
  whatLabels = "par",
  style = "ram",
  layout = "tree",
  residuals = T,
  rotation = 2,
  sizeMan = 7,
  sizeLat = 7,
  color = "lightgray",
  edge.label.cex = 1.2,
  label.cex = 1.3,
  filename = "E:/Visualisasi/tugas/path"
)









# Data --------------------------------------------------------------------
dat = readxl::read_xlsx(
  path = 'D:/Downloads/data_corr.xlsx',
  col_names = TRUE
)

head(dat)
glimpse(dat)

dat$rep = as.factor(data$rep)
dat$water = as.factor(data$water)
dat$priming = as.factor(data$priming)



# Model -------------------------------------------------------------------
mod.id = '
EA =~ aba + apx + pod
YC =~ til + pl + grp + tgw
gy ~ EA + YC
'


library(lavaan)
mod.est = sem(
  model = mod.id,
  data = data
)


summary(mod.est)
summary(mod.est, fit.measures = TRUE)



# Path Diagram ------------------------------------------------------------
library(semPlot)
semPaths(
  object = mod.est,
  what = "path",
  whatLabels = "par"
)

semPaths(
  object = mod.est,
  what = "path",
  whatLabels = "par",
  style = "ram",
  layout = "tree",
  rotation = 2,
  sizeMan = 7,
  sizeLat = 7,
  color = "lightgray",
  edge.label.cex = 1.2,
  label.cex = 1.3
)
