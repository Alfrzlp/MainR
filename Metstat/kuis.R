library(tidyverse)
library(janitor)

# ubah nama kolom agar mudah diproses dgn library janitor
iris = iris %>% clean_names()

# A
anova = aov(sepal_width~species, iris)
summary(anova)

# B
TukeyHSD(anova)

ggplot(iris)+
  geom_boxplot(aes(species, sepal_width))

# C
iris2 = iris %>% 
  filter(species %in% c("setosa", "virginica")) 
#-
by(data = iris2$sepal_width, INDICES = iris2$species, FUN = shapiro.test)

#-
t.test(sepal_length~species, iris2, alternative = "greater", mu = 1)


# D
# membuat fungsi bootstrap dan jacknife sederhana untuk rata2
bootstrap <- function(data, B){
  boot = c()
  for(i in 1:B){
    x = sample(data, replace = T)
    boot = c(boot, mean(x))
  }
  return(mean(boot))
}

jackknife = function(x){
  jack = c()
  for(i in 1:length(x)) jack = c(jack, mean(x[-i]))
  return(mean(jack))
}


# Ambil subset data berbentuk vector
setosa = iris2 %>% filter(species == "setosa") %>% select(sepal_length) %>% pull()
setosa

virginica = iris2 %>% filter(species == "virginica") %>% select(sepal_length) %>% pull()
virginica



# lakukan bootstrap dan jacknife
bootstrap(setosa, 10000)
bootstrap(virginica, 10000)

jackknife(setosa)
jackknife(virginica)

# bukti teknif jacknife adalah teknik yang unbiased
mean(setosa) - jackknife(setosa)
mean(setosa) - jackknife(setosa)
