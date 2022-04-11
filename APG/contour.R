library(mnormt)

x     <- seq(-5, 5, 0.25) 
y     <- seq(-5, 5, 0.25)
mu    <- c(0, 0)
sigma <- matrix(c(2, -1, -1, 2), nrow = 2)
f     <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)

contour(x, y, z)




library(plotly)
plot_ly(z = z, type = "contour", contours = list(showlabels = TRUE)) %>%
  colorbar(title = "Elevation \n in meters")


library(MVQuickGraphs)
xbar <-  matrix(c(95.52,93.39))
Sigma <- matrix(c(3266.46, 1175.50, 1175.50, 474.98),nrow = 2,byrow = T)
n <- 61
p <- nrow(Sigma)

confidenceEllipse(
  X.mean = xbar,
  eig = eigen(Sigma),
  n = n, p = p,
  alpha = 0.05
)

c <- sqrt(qchisq(0.05, 2, lower.tail = F))
# Major (nilai eigen yg besar)
e1
xbar + c * sqrt(lambda[1]) * e[,1]
xbar - c * sqrt(lambda[1]) * e[,1]

# Minor 
xbar + c * sqrt(lambda[2]) * e[,2]
xbar - c * sqrt(lambda[2]) * e[,2]