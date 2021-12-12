
# lib ---------------------------------------------------------------------

library(tidyverse)
library(httr)
library(scales)
library(ggtext)
library(ggfx)
library(colorspace)
library(gggibbous)

resp <- GET("https://data.jabarprov.go.id/api-backend/bigdata/dpmdes/od_jumlah_kepala_desa_berdasarkan_jenis_kelamin_dan_pendidikan?limit>10000")
resp
status_code(resp)

resp_content <- content(resp, as = "parsed", simplifyvector = T)
resp_content$data

resp_content %>% 
  pluck("data") %>% 
  as_tibble_col()

df <- readxl::read_xlsx(r"(D:\Datasets\data.jabarprov.go.id\dpmdes-od_jumlah_kepala_desa_berdasarkan_jenis_kelamin_dan_pendidikan_data.xlsx)")
df


# Data Preprocessing ------------------------------------------------------

to_plot <- 
  df %>% 
  count(
    place = "Jawa Barat",
    jenis_kelamin, wt = jumlah_kepala_desa) %>% 
  mutate(pct = n/sum(n)) %>% 
  pivot_wider(
    names_from = jenis_kelamin,
    names_repair = janitor::make_clean_names,
    values_from = c(n, pct)
  ) %>% 
  mutate(x = 1, y = 1,
         label = str_glue(
           '<p><span style="color:#ffffff"><span style="font-size:36px"><strong>{n_perempuan}</strong></span></span>&nbsp;orang atau<span style="color:#ffffff"><strong><span style="font-size:20px"> {percent(pct_perempuan, accuracy = 0.1)}</span></strong></span><br>
            Kepala desa Perempuan<br>
            <span style="font-size:22px">di&nbsp;<span style="color:#f1c40f"><strong>{place}</strong></span></span></p>'))


to_plot
# Viz ---------------------------------------------------------------------
back_coll <- "#071013"
male_col <- "#002500"
female_col <- "#f0f2A6"
  

to_plot %>% 
  ggplot(aes(x, y)) +
  # gradasi warna
  with_inner_glow(
    geom_moon(aes(ratio = pct_laki_laki, right = F),
              size = 100, colour = NA,
              fill = male_col),
    colour = lighten(male_col, 0.25),
    # melebarkan warna glow
    sigma = 5,
  )+
  with_outer_glow(
    geom_moon(aes(ratio = pct_perempuan), size = 100,
              fill = female_col,
              colour = NA),
    colour = lighten(female_col, 0.25),
    sigma = 10,
  ) +
  geom_richtext(
    # paling bawah
    aes(y = -Inf, label = label),
    family = "Fira Code",
    size = 3.5,
    vjust = "bottom",
    colour = "grey90",
    fill = NA,
    label.colour = NA
  ) +
  theme(
    # warna siku L
    plot.background = element_rect(fill = back_coll,  colour = NA),
    panel.background = element_rect(fill = back_coll,  colour = NA),
    
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )
