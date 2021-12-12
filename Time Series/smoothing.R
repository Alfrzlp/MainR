library(tidyverse)

# Jumlah Barang Melalui Transportasi Kereta Api Menurut Pulau (Ribu Ton)

# Data Wragling -----------------------------------------------------------

s <- '938	749	905	915	775	921	871	-	-	-	-	-	1052	963	1026	936	681	856	936	991	996	956	887	944	1243	975	1169	1158	1203	895	1249	1152	1145	1177	1177	1194	1227	1021	1205	1193	1338	846	1357	1323	1330	1391	1284	1300	974	861	966	967	1101	781	1081	1176	1083	1197	1143	1110	927	734	785	967	873	945	766	1019	936	975	973	991	1023	760	766	768	834	850	587	821	816	959	987	899	760	808	793	704	792	881	684	842	1361	1135	1035	1447	606	551	613	621	669	753	784	501	728	789	1018	667	436	482	510	509	505	646	545	361	618	669	589	608	339	231	341	344	374	382	474	353	349	461	433	508	262	258	286	274	293	642	388	365	191	297	315	288	315	224	262	268	311	374	444	374	272	429	340	363	350	297	347	341	379	357	381	384	270	283	238	337	301	245	292	290	335	335	374	352	356	317	367	360	325	295	323	299	319	341	348	358	344	285	316	349'

jb_ka <- 
  read.table(textConnection(s), header = F) %>% 
  t() %>% 
  as_tibble() %>% 
  mutate(
    bulan = rep(1:12,  16),
    tahun = rep(2021:2006, each = 12)
  ) %>% 
  arrange(tahun, bulan) %>% 
  mutate(date = paste0(tahun,"-",bulan,"-01"),
         date = as.Date.character(date)) %>% 
  select(jb = V1, date) %>% 
  filter(jb != "-") %>% 
  type_convert() 

head(jb_ka)

openxlsx::write.xlsx(
  jb_ka,
  "D:/Datasets/__Time Series/jumlah barang melalui KA jawa.xlsx"
)

# Viz Data ----------------------------------------------------------------
autoplot(jbka) +
  geom_line(y = augment(fit)$.fitted, color = 'steelblue')

autoplot(jbka) +
  geom_line(
    lwd = 1.1
  ) +
  geom_line(
    aes(y = augment(fit)$.fitted),
    color = 'lime', 
    lwd = 1.1
  ) +
  labs(
    x = NULL,
    y = "Ribu (Ton)",
    title = "Jumlah Barang Melalui Transportasi Kereta Api",
    subtitle = "Pulau Jawa Tahun 2006-2021"
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  hrbrthemes::theme_modern_rc(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray30"),
    plot.title = element_text(family = "Arial", face = "bold",
                              colour = "white", size = rel(1.75)),
    plot.subtitle = element_text(family = "Arial",
                                 colour = "white", size = rel(1.25)),
    axis.title.y = element_text(hjust = 1),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = 'gray70'),
    plot.margin = margin(15, 15, 15, 15),
  ) 

plot_jb

# Forecast ----------------------------------------------------------------
library(fpp2)

jb <- ts(jb_ka$jb, frequency = 12)
# MAD <- ME
# Bias <- MAE 
# MAPE 

eval_forecast <- function(y, f){
  data.frame(y, f) %>% 
    mutate(res = y-f) %>% 
    summarise(
      bias = mean(res, na.rm = T),
      mad = mean(abs(res), na.rm = T),
      mse = mean(res^2, na.rm = T),
      mape = mean(abs(res)/y, na.rm = T)*100
    )
}

r_naive <- naive(jb, h = 20)
accuracy(r_naive)

r_snaive <- snaive(jb, h = 10)
accuracy(r_snaive)

# average -----------------------------------------------------------------

ma <- function(df, col, n, metode = "double", jb,...){
  metode <- match.arg(metode, c("double", "single", "weighted"))
  
  if(metode == "double"){
    df <- 
      df %>% 
      mutate(
        avg1 = zoo::rollmeanr({{col}}, k = n, fill = NA),
        avg2 = zoo::rollmeanr(avg1, k = n, fill = NA),
        a = 2*avg1-avg2,
        b = 2*(avg1-avg2)/(n-1)
      ) 
    fit <- df$a + df$b*1
  }else{
    col1 <- deparse(substitute(col))
    if(metode == "single"){
      fit <- TTR::SMA(df[[col1]], n = n)
    }else if(metode == "weighted"){
      fit <- TTR::WMA(df[[col1]], n = n, ...)
    }
  }
  df$fit <- c(NA, fit[-length(fit)]) 
  
  hasil <- 
    df %>% 
    mutate(res = {{col}}-fit) %>% 
    summarise(
      bias = mean(res, na.rm = T),
      mad = mean(abs(res), na.rm = T),
      mse = mean(res^2, na.rm = T),
      mape = mean(abs(res)/jb, na.rm = T)*100
    ) %>% as.data.frame()
  
  return(
    list(df, hasil)
  )
}

ma(jb_ka, jb, 12, "sing")
ma(jb_ka, jb, 3, "double")
ma(jb_ka, jb, 3, "weig", c(0.5, 0.3, 0.2))

# exponential -------------------------------------------------------------

# Simple exponential smoothing
r_se <- ses(jb, initial =  "simple")
autoplot(r_se) +
  autolayer(fitted(r_se), series = "Fitted")

r_se$model
accuracy(r_se)



es <- function(x, metode = 'single', ...) {
  metode <- match.arg(metode, c("double", "single", "holt", 'winter'))
  # trend -> holtz
  # trend & seasonal -> winter
  df <- data.frame(x) 
  if(metode == 'single'){
    df <- df %>% 
      mutate(fit = ses(x, initial =  "simple", ...)$fitted)
  }else if(metode == 'double'){
    df %>% 
      mutate()
  }
  
  hasil <- eval_forecast(df$x, df$fit)
  return(
    list(df, hasil)
  )
}

es(jb)
x <- HoltWinters(jb, beta = F, gamma = F)
x$fitted %>% 
  as.data.frame() %>% 
  mutate(fit = ses(jb, initial =  "simple")$fitted[-1])
  

# Holt Method
library(fpp2)
jb_ka <- openxlsx::read.xlsx("D:/_Datasets/__Time Series/jumlah barang melalui KA jawa.xlsx")
jb <- ts(jb_ka$jb, start = c(2006, 1),
         end = c(2021, 7), frequency = 12)

r_holt <- holt(jb, initial = "simple") 

autoplot(r_holt) +
  autolayer(fitted(r_holt), series = "Fitted")

r_holt$model
r_holt$fitted
accuracy(r_holt)



# winter method
r_winter <- hw(jb) 
autoplot(r_winter) +
  autolayer(fitted(r_winter), series = "Fitted")

r_winter$model
r_winter$fitted
r_winter
accuracy(r_winter)



r_ets <- ets(jb, model = "ZZZ") 
accuracy(r_ets)

autoplot(r_winter) +
  autolayer(fitted(r_winter), series = "Fitted") +
  autolayer(fitted(r_ets), series = "ets")



library(tidyverse)
library(forecast)

# Data --------------------------------------------------------------------
jb_ka <- openxlsx::read.xlsx("D:/_Datasets/__Time Series/jumlah barang melalui KA jawa.xlsx")
jb <- ts(jb_ka$jb, start = c(2006, 1),
         end = c(2021, 7), frequency = 12)


# Exponential -------------------------------------------------------------
se <- HoltWinters(x, beta = F, gamma = F)
se$fitted
accuracy(se$fitted, x)

es(jb, alpha = 0.1)

# Double Exponential ------------------------------------------------------
des <- HoltWinters(x, gamma = F)
des$fitted
accuracy(des$fitted, x)
des
predict(des, 13)

# Winter ------------------------------------------------------------------
win <- HoltWinters(x)
win$fitted
accuracy(win$fitted, jb)

# Forecast ----------------------------------------------------------------
predict(des, 2)
