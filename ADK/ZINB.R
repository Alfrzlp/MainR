library(MASS)
library(pscl)
library(boot)
library(tidyverse)

# download.file('https://stats.idre.ucla.edu/stat/data/fish.csv',
#               destfile = 'D:/_Datasets/fish.csv')

fish <- read.csv("D:/_Datasets/fish.csv")
# livebait <- umpan hidup gak?
# camper <- bawa kamping gak?

# Zero-inflated Poisson regression does better when the data is not overdispersed,
# i.e. when variance is not much larger than the mean

zinb <- within(fish, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)

ggplot(zinb, aes(count, fill = camper)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(camper ~ ., margins = TRUE, scales = "free_y")

m1 <- zeroinfl(count ~ child + camper | persons,
  data = zinb, dist = "negbin", EM = F
)
summary(m1)
