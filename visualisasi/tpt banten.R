library(tidyverse)

tpt <- readxl::read_xlsx("D:/Downloads/tpt.xlsx")
dat <- tpt %>% 
  select(c(1, 3:12))


dat <- dat %>% 
  janitor::clean_names() %>% 
  mutate_at(vars(x2014:x2012), ~ str_replace_all(.x, '\\-', 'NA')) 
  
dat <- dat %>% 
  type_convert() %>% 
  pivot_longer(-1, values_to = "tpt", names_to = "tahun") %>% 
  mutate(
    tahun = str_remove_all(tahun, "x")
  ) %>% 
  type_convert()


PROV <- c("BANTEN", "KEP. RIAU", "DKI JAKARTA", "JAWA BARAT")
PROV <- c("BANTEN")
banten <- dat %>% 
  filter(prov %in% PROV)

dat %>% 
  filter(!prov %in% PROV) %>% 
  ggplot(aes(x = tahun, y = tpt, group = prov, col = 'blue')) +
  geom_line(lwd = 0.4) +
  geom_line(
    data = banten, 
    aes(x = tahun, y = tpt, group = prov, col = prov),
    lwd = 1
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_colour_discrete(labels = c('Banten', "Provinsi Lainnya")) +
  labs(
    x = "Tahun", y = "TPT",
    colour = "Provinsi",
    title = "Tingkat Pengangguran Terbuka (TPT) Berdasarkan Provinsi",
    subtitle = "Tahun 2012 - 2021"
  ) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    plot.title = element_text(face = "bold")
  )




ggsave(
  filename = "E:/Visualisasi/tpt_banten.png",
  width = 8.5,
  height = 5.5,
  dpi = 500,
  scale = 0.75,
  bg = "white"
)
