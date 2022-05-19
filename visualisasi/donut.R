library(webr)

df_bpum <- 
  dm::read_pattern(
    'Penerima BPUM	4965
     Bukan Penerima BPUM	28231',
    pos_non_angka = 1,
    pos_angka = 2,
    col_names = c('v1', 'n')
  ) %>% 
  mutate(v1 = factor(v1)) %>% 
  type_convert() 


df_bpum

PieDonut(
  df_bpum, 
  aes(v1, count = n),
  title = "Kategori Rumah Tangga Usaha Mikro",
  r0 = 0.5
)






# BPUM --------------------------------------------------------------------


hsize <- 3

df_bpum %>% 
  mutate(
    x = 3,
    n = round(n*100/sum(n), 2)
  ) %>% 
  ggplot(aes(x = hsize, y = n, fill = v1)) +
  geom_col(color = "white") +
  geom_text(aes(label = paste(n, '%')), color = 'white',
            fontface = 2,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  xlim(c(1, hsize + 0.5)) +
  scale_fill_manual(values = c("#67CFC5", "#D9B05E")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Kategori Rumah Tangga Usaha Mikro",
    fill = NULL
  ) +
  theme(
    plot.title = element_text(size = rel(1.3), face = "bold"),
    legend.position = c(1.2, 0.8),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )


ggsave(
  filename = str_glue("E:/Visualisasi/desil/bpum.png"),
  width = 10,
  height = 5,
  units = "in", dpi = 300,
  scale = 0.7, bg = 'white'
)


# Status Kemiskinan -------------------------------------------------------
df_sk <- 
  dm::read_string(
    '1	1417	4.27
     2	1859	5.6
     3	2252	6.78
     4	5073	15.28
     5	22595	68.07',
    col_names = c('sk', 'n', 'p')
  ) %>% 
  mutate(
    sk = c('sangat miskin', 'miskin', 'hampir miskin', 'rentan miskin', 'tidak miskin'),
    sk = str_to_title(sk)
  )

sk_levels <- c('sangat miskin', 'miskin', 'hampir miskin', 'rentan miskin', 'tidak miskin')
df_sk %>% 
  ggplot(aes(x = hsize, y = p, fill = factor(sk, levels = str_to_title(sk_levels)))) +
  geom_col(color = "white") +
  geom_text(aes(label = paste0(p, '%')),
            color = c(rep('white', 4), 'slategray'),
            fontface = 2,
            position = position_stack(vjust = 0.55)) +
  coord_polar(theta = "y") +
  xlim(c(1.7, hsize + 0.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Status Kemiskinan",
    fill = NULL
  ) +
  scale_fill_brewer(palette = 'Oranges', direction = -1) +
  # scale_fill_manual(values = c("#A10000", "#BF0404", "#E00000", "#FF5454", "#FF7F7F"))+
  theme(
    plot.title = element_text(size = rel(1.3), face = "bold"),
    legend.position = c(1.2, 0.8),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )


ggsave(
  filename = str_glue("E:/Visualisasi/desil/sk.png"),
  width = 10,
  height = 5,
  units = "in", dpi = 300,
  scale = 0.9, bg = 'white'
)


