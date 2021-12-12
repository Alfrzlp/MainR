pakan1 = c(60.8, 57, 65, 58.6, 61.7)
pakan2 = c(68.7, 67.7, 74, 66.3, 69.8)
pakan3 = c(102.6, 102.1, 100.2, 96.5, 103.5)
pakan4 = c(87.9, 84.2, 83.1, 85.7, 90.3)

data = data.frame(pakan1, pakan2, pakan3, pakan4)
data

data %>% pivot_longer(everything()) 

library(car)
data$name = as.factor(data$name)
leveneTest(value~name, data = data, center = median)


z = z %>% select_if(is.numeric)
#make Zij
z = z %>% mutate_all(~abs(.x-median(.x)))
z %>% as.data.frame()

# make all z bar ij
z %>% summarise_all(mean) %>% 
  as.data.frame()

# make zbar ..
zbar = z %>% summarise_all(sum) %>% sum()/(nrow(z)*ncol(z))
zbar

# bagian atas
30*(z %>% summarise_all(mean)-zbar)^2 %>% sum()

# bagian bawah
z %>% mutate_all(~(.x-mean(.x))^2) %>% 
  summarise_all(sum) %>% sum()


(90-3)*251978774687/(2*578236864331)


pnorm(-1.44)

