library(httr)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(sf)
library(hereR)


get_data <- function(folder = "output/"){
  url = "https://traffic.ls.hereapi.com/traffic/6.2/flow.json?apiKey=MI1Puf0qa1RVDJDTWyNmiPZabg2IqRPPu5CEm6DRsHs&bbox=-6.077811,106.559667;-6.665355,107.146117&responseattributes=sh,fc"
  respone = httr::GET(url)
  data = content(respone, as = "text")
  data = fromJSON(data)
  nama = paste0(folder, gsub(":", "-", substr(Sys.time(), 1, 19)), ".json")
  write_json(data, nama)
}

get_data()
# ==================================
url = "https://traffic.ls.hereapi.com/traffic/6.2/flow.json?apiKey=MI1Puf0qa1RVDJDTWyNmiPZabg2IqRPPu5CEm6DRsHs&bbox=-6.077811,106.559667;-6.665355,107.146117&responseattributes=sh,fc"
respone = httr::GET(url)
http_type(respone)
data = content(respone, as = "parsed")

# harus text baru bisa di jadikan json
data = content(respone, as = "text")
dat = fromJSON(data)




data = fromJSON("Main Python New/viz/flow.json")

data$RWS$RW %>% 
  listviewer::jsonedit()






# tipe 1 =================================================================
olahshape = function(df){
  shape = paste0(df[,1], collapse = "") %>% str_trim()
  shape = paste0(gsub(",", " ", str_split(shape, " ")[[1]]), collapse = ", ")
  shape = paste0("SRID=4326;LINESTRING(",shape,")")
  shape = st_as_sfc(shape)
  
  FC = mean(df[,2])
  return(data.frame(shape, FC))
}

df1 = cbind(
  data[[1]]$FI[[1]]['TMC']$TMC ,
  data[[1]]$FI[[1]]['CF']$CF %>% bind_rows() %>% 
    select(TY, SP, FF, JF, CN),
  lapply(data[[1]]$FI[[1]]['SHP']$SHP, olahshape) %>% bind_rows()
)

df1



library(doSNOW)
library(foreach)
cores = parallel::detectCores()
cl <- makeSOCKcluster(cores-1)
registerDoSNOW(cl)

df1 = resume(data[[1]])
# iterasi secara pararel
df1 <- foreach(i = 2:length(data), .combine = rbind, .packages = c("dplyr", "stringr", "sf"), .multicombine = T) %dopar% {
                  resume(data[[i]])
                 }
stopCluster(cl)



#Tipe 2 ============================================================

data = fromJSON("Main Python New/viz/flow.json")
data = fromJSON("Main R/jatim.json")
flow2 = data$RWS$RW[[1]]$FIS



library(doSNOW)
library(foreach)
cores = parallel::detectCores()
cl <- makeSOCKcluster(cores-1)
registerDoSNOW(cl)

df = resume(flow2[[1]])
df
# iterasi secara pararel
start = Sys.time()
df <- foreach(i = 2:length(flow2), .combine = rbind, .packages = c("dplyr", "stringr", "sf"), .inorder = F) %dopar% {
  resume(flow2[[i]])
}
print(Sys.time()-start)
stopCluster(cl)
# CN = 
# QD = arah lalu lintas
# JF = tingkat kemacetan


df = df %>% 
  mutate(id = paste0(PC, QD), .keep = "unused", .before = "DE") %>% 
  mutate(group = cut(JF, c(-1,1,3,4,8,11), c(1,2,3,4,5)),
         col = case_when(group == 1~'#034732',
                         group == 2~'#008148',
                         group == 3~'#069E2D',
                         group == 4~'#F5BB00',
                         group == 5~'#FB5012'))

#2:'#069E2D',0:'#034732',1:'#008148',3:'#F5BB00',4:'#FB5012'
head(df)
st_as_sf(df, coords=c("long","lat")) %>% 
  group_by(id) %>% 
  dplyr::summarise()

df = st_as_sf(df, coords=c("long","lat")) 

df1 = df %>%   
  group_by(id) %>% 
  dplyr::summarise() %>% 
  st_cast("MULTILINESTRING") %>% 
  st_set_crs(4326) 

df %>% 
  select(-geometry)


df %>% 
  arrange(group) %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
  setView(lng = 106.828303, lat = -6.223996, 11) %>% 
  #fitBounds(106.559667, -6.077811, 107.146117, -6.665355) %>% 
  addPolylines(color = ~col, opacity = 2, weight = ~5/FC)







map <- leaflet() %>% 
  addProviderTiles("CartoDB.DarkMatter") %>% 
  #setView(lng = 106.828303, lat = -6.223996, 10) %>% 
  #fitBounds(106.559667, -6.077811, 107.146117, -6.665355)
  setView(lat = -7.461942104558447, lng = 112.69259594982285, 10) %>% 
  fitBounds(111.906528, -6.775616, 113.70357436384134, -8.162503171095738)





























sub(".*(?<=,)([0-9]+.[0-9]+).*", replacement = "\\1", perl = T, shape)


resume <- function(df){
  # ambil banyak baris 
  n = sapply(lapply(df$FI[[1]]['SHP']$SHP, olahshape), nrow)
  data1 = cbind(
    df$FI[[1]]['TMC']$TMC ,
    df$FI[[1]]['CF']$CF %>% bind_rows() %>% select(TY, SP, FF, JF, CN)
  ) %>% 
    # copy baris
    slice(rep(1:n(), n))
  
  shp = df$FI[[1]]['SHP']$SHP
  data2 = lapply(df$FI[[1]]['SHP']$SHP, olahshape) %>% bind_rows()
  
  cbind(data1, data2)
}

olahshape = function(df){
  shape = paste0(df[,1], collapse = "") %>% str_trim()
  shape = str_split(shape, " ")[[1]]       
  lat = str_extract(shape, "(?<!,)(-[0-9]+.[0-9]+)") %>% as.numeric()
  long = str_extract(shape, "(?<=,)([0-9]+.[0-9]+)") %>% as.numeric()
  
  FC = mean(df[,2])
  return(data.frame(long, lat, FC))
}

resume(data[[1]])




