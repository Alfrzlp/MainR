library(MASS)
library(VGAM)
library(lmtest)
library(modelsummary)
library(kableExtra)


# Data --------------------------------------------------------------------
df_metpen <- 
  readxl::read_xlsx('D:/__SEMESTER 5/Metode Penelitian/data.xlsx') %>% 
  `colnames<-`(c('kab', 'ak','tk', 'rb', 'kp')) %>% 
  type_convert() %>% 
  drop_na()

head(df_metpen)

# -------------------------------------------------------------------------
gpm <- vglm(ak ~ tk + rb + kp, genpoisson2, data = df_metpen, model = T)

models <- list(
  "Poisson" = glm(ak ~ tk + rb + kp, data = df_metpen, family = poisson),
  "Negative Binomial" = glm.nb(ak ~ tk + rb + kp, data = df_metpen) ,
  "Generalized Poisson" = gpm
)

#modelsummary(models, output = "E:/Visualisasi/table.png")

modelsummary(
  models, output = "kableExtra", fmt = "%.4f",
  estimate  = "estimate",
  statistic = '{std.error} ({p.value}) [{conf.low}, {conf.high}]', 
  conf_level = .95
)

gm <- list(
  list("raw" = "nobs", "clean" = "N obs", 'fmt' = "%.3f"),
  list("raw" = "AIC", "clean" = "aic", 'fmt' = "%.3f"),
  list("raw" = "BIC", "clean" = "bic", 'fmt' = "%.3f"),
)



rows <- 
  data.frame(
    term = names(models),
    Loglik = c(logLik(pm)[1], logLik(nbm)[1], logLik(gpm)[1]),
    LRtest = c(lrtest(pm)$`Pr(>Chisq)`[2], lrtest(nbm)$`Pr(>Chisq)`[2], lrtest_vglm(gpm)@Body$`Pr(>Chisq)`[2])
  ) %>% 
  mutate(
    LRtest = LRtest
  ) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(-1) %>% 
  `colnames<-`(c('term', names(models))) %>% 
  as_tibble()

#attr(rows, 'position') <- c(30)
rows

tab <- 
 modelsummary(
  models, output = "kableExtra", fmt = "%.4f",
  estimate  = "estimate",
  statistic = c("conf.int",
                "<b>s.e</b>. = {std.error}", 
                "<b>t-statistic</b> = {statistic}",
                "<b>p-value</b> = {p.value}"), 
  align = "lrrr",
  conf_level = .95,
  gof_omit = 'R2|R2 Adj.|F|RMSE|Log.Lik.',
  # gof_map = gm,
  add_rows = rows,
  coef_rename = c('tk' = 'Total Kendaraan Bermotor',
                  'rb' = 'Panjang Jalan Rusak Berat (Km)',
                  'kp' = 'Kepadatan Penduduk',
                  '(Intercept):2' = 'Dispersion Parameter',
                  '(Intercept):1' = '(Intercept)'),
  notes = list('<b>Sumber  : jateng.bps.go.id</b>', 
               'Kelompok 3 : Bencana'),
  title = '<h2>Perbandingan antar Model</h2>'
) 

tab %>% 
  row_spec(21, background = 'lightblue') %>% 
  row_spec(24, color = 'red') 


get_estimates(models[[1]])




# -------------------------------------------------------------------------
b <- list(geom_vline(xintercept = 0, color = 'orange'),
          annotate("rect", alpha = .1,
                   xmin = -.5, xmax = .5, 
                   ymin = -Inf, ymax = Inf),
          geom_point(aes(y = term, x = estimate), alpha = .3, 
                     size = 10, color = 'red', shape = 'square'))

modelplot(models, coef_omit = '(Intercept)') +
  theme_bw()
modelplot(models, background = b)


# -------------------------------------------------------------------------
dat %>% str
dat %>% 
  mutate_at(1:3, ~ fct_rev(as.factor(.x))) %>% 
  str
