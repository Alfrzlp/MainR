df <- readxl::read_xlsx("D:/Datasets/Data01-website.xlsx")

# 1.  plot
ggplot(df, aes(x = X1, y = Y))+
  geom_point()+
  theme_bw()

ggplot(df, aes(x = X2, y = Y))+
  geom_point()+
  theme_bw()


model <- lm(Y~X1+X2, df)
summary(model)

library(olsrr)
#All Possible Regression (Best Subset)
ols_step_best_subset(model)
# stepwise
ols_step_both_p(model, pent = 0.2)

df <- df %>% 
  mutate_all(~log(.x))

# 2.  uji lack of fit
# apakah model liniear cocok
m1 = lm(Y~X1+X2, df)
m2 = lm(Y~factor(X1)+factor(X2), df)

anova(m1, m2)
#tolak Ho - model liniear tidak cocok


# 3. mencoba menambah interaksi
m1 = lm(Y ~ X1 + X2 + X1*X2, df)
m2 = lm(Y ~ factor(X1) + factor(X2) + factor(X1*X2), df)

anova(m1, m2)
# model interaksi cocok
model <- lm(Y ~ X1 + X2 + X1*X2, df)


# 4. Uji Asumsi
# 4.1 Normalitas
shapiro.test(model$residuals)
nortest::lillie.test(model$residuals)
# gagal tolak Ho - Normal

# 4.2 multikolinearitas
library(car)
car::vif(model)


# 4.3 heteroskedatisitas
library(lmtest)
bptest(model)
# gagal tolak -> tidak terjadi heteroskedastisitas


# 4.4 Autokorelasi
# buka time seris
durbinWatsonTest(model)
lmtest::dwtest(model)

# mencoba mengatasi
cochrane_orcutt(model)

# amatan berpengaruh ========================================
# DFFITS
p = ols_plot_dffits(model)
index_ab <- p$data %>% 
  dplyr::filter(color == "outlier") %>% 
  pull(obs)

# outlier ===================================================
# deleted studentized residual 
# outlier y
p = ols_plot_resid_stud_fit(model)
index_outlier <- p$data %>% 
  dplyr::filter(color == "outlier") %>% 
  pull(obs)

index <- union(index_ab, index_outlier)
df <- df[-c(index),] 



str <- '1 13.8 Pria LK A 10 91.47
2 15.8 Wanita L B 18 94.38
3 18 Wanita LK A 19 93.65
4 13 Wanita AA A 20 91.64
5 15 Wanita AA C 30 92.54
6 14.9 Wanita AA C 21 92.06
7 14.5 Pria LK A 24 94.76
8 14.2 Wanita AA B 10 93.05
9 15.9 Pria LK C 13 92.73
10 14.7 Wanita L A 20 93.14
11 13.4 Pria AA A 23 94.42
12 15.3 Pria L C 30 92.36
13 13.5 Wanita AA A 11 94.29
14 14.2 Pria AA B 22 93.81
15 14.2 Pria AA B 12 93.84
16 15.1 Pria LK B 17 93.15
17 15.1 Wanita L A 28 95.05
18 14 Pria AA B 13 92.73
19 14.8 Wanita L A 31 93.56
20 13.4 Wanita AA A 27 93.54
21 14.6 Wanita L A 12 92.71
22 14.5 Pria AA B 16 95.18
23 15 Wanita AA C 22 92.66
24 16.1 Pria LK C 23 93.7
25 16.2 Wanita LK A 12 93.75
26 15.1 Pria AA C 18 93.96
27 15.9 Pria LK C 20 92.88
28 16.5 Wanita L C 25 93.37
29 14.9 Pria AA C 10 92.84
30 13.7 Wanita AA A 19 95.12'

library(fastDummies)

df <- read.table(textConnection(str), header = F) %>% 
  select(-V1) %>% 
  `colnames<-`(c('PD', 'JK', 'Jabatan', 'Prodi', 'LB', 'PK')) %>% 
  janitor::clean_names() 

unique(df$jk)
unique(df$prodi)

df <- df %>% 
  mutate(jk = if_else(jk == "Pria", 1, 0)) %>% 
  dummy_cols("jabatan", remove_selected_columns = T) %>% 
  dummy_cols("prodi", remove_selected_columns = T) %>% 
  select(-c(jabatan_L, prodi_A))

df

model <- lm(pd~jk+pk+jabatan_AA+jabatan_LK+prodi_B+prodi_C, df) 
summary(model)
# Mencari model terbaik
ols_step_both_p(model, details = T, pent = 0.2)
ols_step_backward_p(model)




# No 3 =================================================
df <- foreign::read.spss("D:/Datasets/Data03-susu.sav",
                         to.data.frame = T) %>% 
  select(-id)
df

model <- lm(currentm~., df)

hasil <- ols_step_all_possible(model, aic = T)
data.frame(N = hasil$n,
           predictors = hasil$predictors,
           Rsquare = hasil$rsquare,
           MSEp = hasil$msep,
           adj_Rsquare = hasil$adjr,
           cp = hasil$cp,
           aic = hasil$aic) %>% 
  arrange(adj_Rsquare, -MSEp, -aic, -cp)

# model terpilih
model <- lm(currentm~previous+protein+days+lactatio, df)



# 4. Uji Asumsi
ols_plot_diagnostics(model)

# 4.1 Normalitas
shapiro.test(model$residuals)
nortest::lillie.test(model$residuals)

ols_test_normality(model)
# gagal tolak Ho - Normal

# 4.2 multikolinearitas
library(car)
car::vif(model)


# 4.3 heteroskedatisitas
library(lmtest)
bptest(model)
gqtest(model)
# gagal tolak -> tidak terjadi heteroskedastisitas


# 4.4 Non Autokorelasi
# bukan time seris
durbinWatsonTest(model)
# gagal tolak -> tidak terjadi autokorelasi



# mengatasi tidak normal

# amatan berpengaruh =============
# DFFITS
p = ols_plot_dffits(model)
index_ab <- p$data %>% 
  dplyr::filter(color == "outlier") %>% 
  pull(obs)

# outlier ========================
# deleted studentized residual 
# outlier y
p = ols_plot_resid_stud_fit(model)
index_outlier <- p$data %>% 
  dplyr::filter(color == "outlier") %>% 
  pull(obs)

index <- union(index_ab, index_outlier)
df <- df[-c(index),] 



partial.R2(lm(pd~jk, df), lm(pd~pk+jk, df))
