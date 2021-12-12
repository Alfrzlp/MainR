str <- '65	72	85	60	69 80	78	88	68	71 82	72	75	63	74
45	40	48	50	54 54	56	47	56	38
26	23	30	32	28
'

str = '55 60 54 80 60
49 80 75 56 50 57 53
60 80 67 76 56 64
34 65 54 47
76 56 78 84 65 58 76 75
57 65 50 68 56 71'

# stratified double sampling =================================
get_allCluster <- function(str, ...){
  all_cluster = strsplit(str, split = "\n")[[1]] 
  all_cluster = lapply(all_cluster, str2vec, ...) 
  return(all_cluster)
}
st_double <- function(data, N, nh_){
  if(class(data) == "character") data = get_allCluster(data)
  else data = data
  
  n_ = sum(nh_)
  g = (N-sum(nh_))/(N-1)
  
  df <- lapply(data, function(x) {
    data.frame(nh = length(x),
               ybarh = mean(x),
               sh2 = var(x))
  }) %>% 
    bind_rows() %>% 
    mutate(wh = nh_/n_)
  
  ybarstd = sum(df$wh*df$ybarh)
  
  ## (wh^2-g*wh/n')*sh^2/nh  + g*wh(ybar h- ybar std)^2/n'
  df <- df %>% 
    mutate(nh_, .before = nh) %>% 
    mutate(k = (wh^2-g*wh/n_)*sh2/nh + g*wh*(ybarh-ybarstd)^2/n_)
  
  vybar_std = n_*sum(df$k)/(n_-1)
  vycap_std = vybar_std*n_^2
  
  
  return(list(
    n_aksen = n_, g = g, df,
    rata2 = data.frame(Estimasi = c(ybarstd, vybar_std, sqrt(vybar_std), sqrt(vybar_std)*100/ybarstd)) %>% 
      `rownames<-`(c('ybar std', 'v(ybar std)', 'se(ybar std)', 'rse(ybar std)')),
    Total = data.frame(Estimasi = c(ybarstd*n_, vycap_std, sqrt(vycap_std), sqrt(vycap_std)*100/(ybarstd*n_))) %>% 
      `rownames<-`(c('ycap std', 'v(ycap std)', 'se(ycap std)', 'rse(ycap std)'))
  ))    
} 






# contoh 1 ==============================
data = get_allCluster(str)
N = 150
nh_ = c(55, 40, 25)
st_double(data, N, nh_)  

# contoh 2
data = get_allCluster(str)
hasil = st_double(data, N = 10000, nh_ = c(50, 75, 60, 43, 80, 58))
hasil

# unequal =====================================
c = 100
c0 = 0
c1 = 1
c1_ = 0.02

hasil[[3]] %>% 
  mutate(wy = wh*(ybarh-hasil$rata2$Estimasi[1])^2,
         ws = wh*sqrt(sh2)) %>% 
  summarise(sum(wy), sum(ws)) 

wy = 39.23679
ws = 10.51083
# n'
(c-c0)*sqrt(wy)/sqrt(c1_)/(sqrt(c1*wy) + sqrt(c1_)*ws)
# n
hasil[[3]] %>% 
  mutate(wh*sqrt(sh2), n = (c-c0)*wh*sqrt(sh2)/sqrt(c1)/(sqrt(c1*wy) + sqrt(c1_)*ws))


(sqrt(c1*wy) + sqrt(c1_)*ws)^2/(c-c0)







# equal ========================================
# Vn
vn = (sum(hasil[[3]]$wh*sqrt(hasil[[3]]$sh2)))^2
vn
# Vn'
vn_ = sum(hasil[[3]]$wh*(hasil[[3]]$ybarh-hasil$rata2$Estimasi[1])^2)
vn_

c = 100
c0 = 0
c1_ = 0.02
c1 = 1

# n'
(c-c0)*sqrt(vn_*c1)/(sqrt(c1*c1_)*(sqrt(c1*vn)+sqrt(c1_*vn_)))
# n
(c-c0)*sqrt(vn*c1_)/(sqrt(c1*c1_)*(sqrt(c1*vn)+sqrt(c1_*vn_)))


  
# double sampling penduga ratio, beda, regresi ==========================
double_ <- function(df, N, n_, xbar_, metode = 'ratio', k = NULL) {
  metode <- match.arg(tolower(metode), c('ratio', 'regresi', 'beda', 'srs'))
  ybar = mean(df$y)
  xbar = mean(df$x)
  n = nrow(df)
  
  if(metode == 'ratio') k = ybar/xbar
  else if(metode == 'regresi') k = lm(y~x, df)$coefficients[2] %>% `names<-`(NULL)
  else if(metode == 'srs') k = 0
  else if(metode == 'beda'){
    if(is.null(k)) stop('Masukkan Nilai C')
  }else{
    stop('Metode tidak diketahui')
  }
  
  sy2 = var(df$y)
  sx2 = var(df$x)
  
  ybar_d = ybar + k*(xbar_ - xbar)
  vybar_d = (1/n_ - 1/N)*sy2 +
    (1/n - 1/n_)*(sy2 + k^2*sx2 - 2*k*cor(df$x,df$y)*sqrt(sx2*sy2))
  
  seybar_d = sqrt(vybar_d)
  rseybar_d = seybar_d*100/ybar_d
  
  return(list(
    data.frame(nilai = c(ybar, xbar, xbar_, sx2, sy2, k)) %>% 
      `rownames<-`(c('ybar', 'xbar', "xbar'", 'sx^2', 'sy^2', 'k')),
    data.frame(Sampel = c(N, n_, n)) %>% 
      `rownames<-`(c('N', "n'", 'n')),
    rata2 = data.frame(Estimasi = c(ybar_d, vybar_d, seybar_d, rseybar_d)) %>% 
      `rownames<-`(c('ybar d', 'v(ybar d)', 'se(ybar d)', 'rse(ybar d)')),
    Total = data.frame(Estimasi = c(ybar_d*N, vybar_d*N^2, sqrt(vybar_d)*N, rseybar_d)) %>% 
      `rownames<-`(c('ycap std', 'v(ycap std)', 'se(ycap std)', 'rse(ycap std)')),
    Metode = paste('Double Sampling', metode)
  ))   
}

str <- 'x y
120	230
150	256
135	250
140	250
152	278
120	298
135	303
145	260
150	300
205	260
190	275
180	305
'

df = read.table(textConnection(str), header = T)

N = 30
X = 4467
double_(df, N = 30, n_ = 25, xbar_ = 186.125, metode = 'srs')

