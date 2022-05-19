my_colors <- c("#CDA351", "#E4CFA1", "#49A59B", "#9BCFC9")
font_col <- "white"
# Data --------------------------------------------------------------------
datinf <- readxl::read_xlsx('D:/Downloads/data infografis.xlsx')
glimpse(datinf)


datinf %>% 
  dplyr::select(-c(Timestamp, Nama)) %>% 
  rename(jk = `Jenis Kelamin`) %>% 
  group_by(jk) %>% 
  count()

datinf %>% 
  dplyr::select(Nama, jk = `Jenis Kelamin`) %>% 
  dplyr::filter(jk == 'Laki-laki') 


datinf <- datinf %>% 
  dplyr::select(-c(Timestamp, Nama)) %>% 
  rename(jk = `Jenis Kelamin`) %>% 
  pivot_longer(-1) %>% 
  group_by(name, value, jk) %>% 
  summarise(
    p = n()
  ) %>% 
  mutate(
    p = ifelse(jk == 'Laki-laki', p*100/14, p*100/20),
    gp = case_when(
      value == 'Tidak Pernah' & jk == 'Laki-laki' ~ 1,
      value == 'Pernah' & jk == 'Laki-laki' ~ 2,
      value == 'Tidak Pernah' & jk == 'Perempuan' ~ 3,
      value == 'Pernah' & jk == 'Perempuan' ~ 4
    )
  )


datinf %>% 
  dplyr::filter(
    jk == 'Laki-laki',
    value == 'Pernah'
  ) %>% 
  as.data.frame()

datinf %>% 
  ggplot(
    aes(x = p, y = factor(name), fill = factor(gp), label = paste0(round(p, 2), " %"))
  ) +
    geom_bar(data = subset(datinf, jk == 'Perempuan'), aes(x = -p), stat = "identity", width = 0.9) +
    geom_bar(data = subset(datinf, jk == 'Laki-laki'), stat = "identity", width = 0.9) +
    geom_text(
      data = datinf %>%
        dplyr::filter(
          jk == 'Perempuan',
          value == 'Pernah'
        ),
      aes(x = -p), color = font_col, fontface = 2, nudge_x = -10
    ) +
    geom_text(
      data = datinf %>%
        dplyr::filter(
          jk == 'Laki-laki',
          value == 'Pernah'
        ),
      aes(x = p), color = font_col, fontface = 2, nudge_x = 10
    ) +
    scale_y_discrete(
      labels = function(x) str_to_title(x)
    ) +
    scale_x_continuous(labels = my_labels, expand = expansion(mult = c(0.015, 0))) +
    scale_fill_manual(NULL, labels = c(0, 1, 0, 1), values = my_colors) +
    labs(
      title = NULL,
      x = "Persentase",
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title.position = "plot",
      # top,right,bottom,left
      plot.title = element_text(size = rel(1.75), face = "bold", margin = margin(0, 0, 10, 10)),
      axis.title.x = element_text(colour = "black", size = 13.5, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(colour = "black", size = 13.5, margin = margin(0, 15, 0, 0)),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 11),
      panel.grid = element_blank(),
      legend.position = "none"
    )
  
ggsave(
  filename = "E:/Visualisasi/infografis.png",
  width = 10,
  height = 5,
  units = "in", dpi = 300,
  scale = 0.85, bg = 'white'
)
