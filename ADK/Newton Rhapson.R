
# Data --------------------------------------------------------------------
tab3 <- matrix(c(76, 55, 24, 40, 31, 45, 13, 14, 25, 45, 
                 46, 8, 30, 18, 33, 24, 24, 49), nrow = 3)

df <- 
  as.data.frame(tab3) %>% 
  mutate(usia = c('bb', 'Gen X', 'Gen Milenial'),
         .before = V1) %>% 
  `colnames<-`(c('U','ikut_tinggi', 'ikut_sedang', 'ikut_rendah',
                 'tdkikut_tinggi', 'tdkikut_sedang', 'tdkikut_rendah')) %>% 
  pivot_longer(2:7, values_to = 'Freq') %>% 
  separate(name, into = c('K', 'P'), sep = '_') %>% 
  mutate(U = fct_relevel(U, c('Gen Milenial', 'Gen X', 'bb')),
         K = fct_relevel(K, c('tdkikut', 'ikut')),
         P = fct_relevel(P, c('rendah', 'sedang', 'tinggi'))) 


# preprocessing -----------------------------------------------------------

df <- df %>% mutate_at(-4, ~as.numeric(.x))
df

x = as.matrix(data.frame(1, df[,1:3]))
y = as.matrix(df[,4])
n = length(y)
ones = matrix(rep(1,n),ncol=1)
yb = log(y)


(beta <- solve(t(x)%*%x)%*%t(x)%*%yb)
i <- 0; err <- 1

while (sum(err^2) > 10^-10) {
  i <- i + 1
  
  #ll = t(y)%*%x%*%beta-t(exp(x%*%beta))%*%ones
  grad = t(x)%*%y-t(x)%*%(exp(x%*%beta))
  w <- `diag<-`(matrix(0, n, n), exp(x%*%beta))
  hess=-(t(x)%*%w%*%x)
  
  beta_new <- beta - solve(hess)%*%grad
  err <- abs(beta_new - beta)
  beta <- beta_new
  
  cat('Iterasi ke ', i, '\n')
  print(beta_new)
  # print(paste('error ', err))
}

glm(Freq ~ U + K + P, data = df, family = poisson) %>% 
  summary()

se <- sqrt(-diag(solve(hess)))
beta/se


# Regresi Logistik --------------------------------------------------------
(dat <- read.csv('D:/_Datasets/diabetes.csv') %>% 
   janitor::clean_names())


x <- as.matrix(data.frame(1, dat$glucose))
y <- as.matrix(dat$diabetes)
n <- length(y)

# yb = sigmoid(y)
# (beta <- solve(t(x)%*%x)%*%t(x)%*%yb)

beta <- matrix(c(0, 0), 2, 1)
i <- 0; err <- 1
while (sum(err^2) > 10^-10) {
  i <- i + 1
  # Hitung Probabilitas
  p <- sigmoid(x%*%beta)
  grad <- t(x)%*%(y - p)
  
  # H = t(x) * D * X
  # Dimana D adalah diag(p(1-p))
  D <- `diag<-`(matrix(0, n, n), p*(1-p))
  hess <- - (t(x)%*%D%*%x)
  
  beta_new <- beta - solve(hess)%*%grad
  err <- abs(beta_new - beta)
  beta <- beta_new
  
  cat('Iterasi ke :', i, '\t\t','Error :',sum(err^2),'\n')
  print.table(`colnames<-`(beta_new, 'koef'), digits = 6)
  cat('\n\n')
}



se <- sqrt(-diag(solve(hess)))
beta/se

glm(diabetes ~ glucose, family = binomial(), data = dat) %>% 
  summary()
