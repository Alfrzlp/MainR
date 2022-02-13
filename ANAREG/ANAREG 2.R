# seleksi model ======================
df <- readxl::read_xlsx("D:/Datasets/Data01-website.xlsx")
model <- lm(Y ~ X1 + X2, df)

library(olsrr)
# All Possible Regression (Best Subset)
ols_step_best_subset(model)
View(ols_step_best_subset(model))

ols_step_all_possible(model)
# Tampilan yang lebih ramah di mata

# Forward Selection
ols_step_forward_p(model)
# Tabel yang ditampilkan adalah variabel yang DIMASUKKAN dalam model

# Backward Selection
ols_step_backward_p(model)
# Tabel yang ditampilkan adalah variabel yang DIBUANG dalam model

# Stepwise Regression
ols_step_both_p(model)
# Tabel yang ditampilkan adalah variabel yang DIMASUKKAN/DIBUANG (sesuai keterangan) dalam model

# Amatan berpengaruh =================================

# cook distance
ols_plot_cooksd_bar(model)
ols_plot_cooksd_chart(model)

# DFFITS
ols_plot_dffits(model)

# DFBETAS
ols_plot_dfbetas(model)


# outlier ============================================

# studentized residual
p <- ols_plot_resid_stud(model)
p$data
ols_plot_resid_stand(model)

# deleted studentized residual
# outlier y
ols_plot_resid_stud_fit(model)


# Multikolinearitas =================================
library(car)
vif(model)

library(lmridge)
rmod <- lmridge(Girth ~ ., data = df, K = seq(0, 1, 0.001)) # model ridge
plot(rmod) # ridge trace plot
vif(rmod) # vif model ridge
rmod # nilai koef ridge

rmod2 <- lmridge(Girth ~ ., data = df, K = 0.003) # model ridge dengan c terpilih
summary(rmod2)

vif(rmod2)
# heterosketastisitas ===============================
library(lmtest)
# bp test
bptest(model)
# gagal tolak homoskedastisitas

# spearman
spearman.test <- function(model) {
  rs <- cor.test(abs(model$residuals), model$model$x,
    method = "spe"
  )$estimate %>%
    `names<-`(NULL)
  return(rs * sqrt(nrow(model$model) - 2) / sqrt(1 - rs^2))
}
spearman.test(model)

# park test
park.test <- function(model) {
  var <- all.vars(model$call$formula)
  hasil <- model$model %>%
    select(-var[1]) %>%
    mutate_all(~ log(.x)) %>%
    mutate(ln_ei2 = log(model$residuals^2)) %>%
    lm(ln_ei2 ~ ., .) %>%
    summary()
  print(hasil)
  return(hasil$coefficients[2, 3])
}
park.test(model)

# wls
x <- as.matrix(data.frame(1, df$x))
y <- as.matrix(df$y)
w <- matrix(0, nrow = nrow(x), ncol = nrow(x)) %>%
  `diag<-`(1 / df$x)

t(x) %*% w %*% x
solve(t(x) %*% w %*% x)

t(x) %*% w %*% y

b <- solve(t(x) %*% w %*% x) %*% t(x) %*% w %*% y
b

# test
lm(y ~ x, df, weights = 1 / x) %>%
  spearman.test()
# t (1-alpha/2, n-2)

lm(y ~ x, df, weights = 1 / x) %>%
  park.test()



# autokorelasi ==================
library(car)
durbinWatsonTest(model)
# ho tidak ada autokorelasi

model$model %>%
  mutate(
    e = model$residuals,
    e^2,
    `(et-et-1)^2` = (c(NA, e[-length(e)]) - e)^2
  ) %>%
  janitor::adorn_totals("row") %>%
  rowid_to_column("t") %>%
  mutate_if(is.numeric, ~ round(.x, 6)) %>%
  copy2c()
summarise_all(sum)

dw <- function(model, ...) {
  model$model %>%
    mutate(
      e = model$residuals,
      e^2,
      `(et-et-1)^2` = (c(NA, e[-length(e)]) - e)^2
    ) %>%
    mutate_if(is.numeric, ~ round(.x, 6)) %>%
    janitor::adorn_totals("row") %>%
    # rowid_to_column('t') %>%
    return()
}

dw(hasil$model_baru, 6)

# cochrane.orcutt
cochrane_orcutt <- function(model, r = NULL) {
  e <- model$residuals
  data <- model$model %>%
    mutate(e,
      `et*et-1` = c(NA, e[-length(e)]) * e,
      `e^2(t-1)` = c(NA, e[-length(e)])^2
    )

  if (is.null(r)) {
    r <- sum(data$`et*et-1`, na.rm = T) / sum(data$`e^2(t-1)`, na.rm = T)
  }

  y <- data$y[-1] - r * data$y[-nrow(data)]
  x <- data$x[-1] - r * data$x[-nrow(data)]

  newmodel <- lm(y ~ x, data.frame(y, x))
  #
  # cat('\nModel asal Ycap = ', round(newmodel$coefficients[1]/(1-r), 4),
  #     ' + ', round(newmodel$coefficients[2], 4), 'X\n')

  return(list(
    data = data %>% janitor::adorn_totals("row"),
    r = r,
    "Nilai x' dan y'" = data.frame(x, y),
    model_baru = newmodel,
    ujibaru = durbinWatsonTest(newmodel),
    paste(
      "Kembali ke Model asal Ycap = ", round(newmodel$coefficients[1] / (1 - r), 4),
      " + ", round(newmodel$coefficients[2], 4), "X"
    )
  ))
}

hasil <- cochrane.orcutt(model, r = 1)
hasil$`Nilai x' dan y'` %>%
  mutate_if(is.numeric, ~ round(.x, 6)) %>%
  copy2c()



library(orcutt)
orcutt::cochrane.orcutt(model, convergence = 1)

library(HoRM)
hildreth.lu(y = df$y, x = df$x, rho = 0.9)

model$model %>%
  mutate(
    e = model$residuals,
    e^2,
    `(et*et-1)` = (c(NA, e[-length(e)]) * e)
  ) %>%
  janitor::adorn_totals("row") %>%
  rowid_to_column("t") %>%
  mutate_if(is.numeric, ~ round(.x, 6)) %>%
  copy2c()
summarise_all(sum)





# linearitas --------------------------------------------------------------
# gagal tolak normal
shapiro.test(model$residuals)

















df <- read.img(bahasa = "numbers", to_df = T)
df <- rbind(
  df[1:3, ] %>% t(),
  df[4:6, ] %>% t()
) %>%
  as.data.frame() %>%
  remove_rownames() %>%
  select(-1) %>%
  `colnames<-`(c("x", "y"))

model <- lm(y ~ x, df)
summary(model)

df %>%
  mutate(e = model$residuals) %>%
  rowid_to_column("t") %>%
  ggplot() +
  geom_point(aes(y = t, x = e)) +
  labs(
    title = "Residual Plot",
    y = "Time", x = "Residual"
  ) +
  theme_bw()


read.img()
