# https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.statalist.org%2Fforums%2Fforum%2Fgeneral-stata-discussion%2Fgeneral%2F1403539-stacked-bar-single-chart-for-multiple-variables&psig=AOvVaw1FqVgJNWAgzh6QKOYXWh4A&ust=1648620344950000&source=images&cd=vfe&ved=0CAsQjRxqFwoTCLjHi8HT6vYCFQAAAAAdAAAAABAI
s <- "Kulkas (1: ada, 0: tidak)	64,1	74,28
AC (1: ada, 0: tidak)	6,88	5,26
PC/komputer (1: ada, 0: tidak)	18,81	20,81
Motor (1: ada, 0: tidak)	80,88	85,44
Mobil (1: ada, 0: tidak)	15,44	12,23
"

df2 <- dm::read_pattern(
    s,
    pos_non_angka = 1,
    pos_angka = 2:3
  ) %>%
  extract(
    v1,
    into = c("nama", "status"),
    regex = "(\\X*)\\s\\((\\X*)\\)"
  ) %>%
  mutate_at(
    3:4, ~ str_replace_all(.x, ",", ".")
  ) %>%
  type_convert() %>%
  rename(
    bkn_penerima = v2,
    penerima = v3
  ) %>%
  mutate(
    urutan = 1:n(),
    group = 1
  ) %>% 
  dplyr::select(-status)

df2

df_final2 <- df2 %>%
  add_row(
    df2 %>%
      mutate_at(2:3, ~ 100 - .x) %>%
      mutate(group = 0)
  ) %>%
  pivot_longer(2:3, names_to = "status", values_to = "persentase") %>%
  mutate(
    gp = case_when(
      group == 0 & status == "penerima" ~ 1,
      group == 1 & status == "penerima" ~ 2,
      group == 0 & status == "bkn_penerima" ~ 3,
      group == 1 & status == "bkn_penerima" ~ 4
    )
  )

df_final2

my_labels <- paste0(c(seq(100, 0, -50), seq(50, 100, 50)), "%")
my_labels
my_colors <- c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")



bkn_penerima2 <- df_final2 %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  geom_bar(data = subset(df_final2, status == "bkn_penerima"), stat = "identity") +
  scale_fill_manual("Bukan Penerima BPUM", labels = c('Ada', 'Tidak ada'), values = my_colors[3:4])
bkn_penerima2


penerima2 <- df_final2 %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  geom_bar(data = subset(df_final2, status == "penerima"), stat = "identity") +
  scale_fill_manual("Penerima BPUM", labels = c('Ada', 'Tidak ada'), values = my_colors[1:2])
penerima2




df_final2
windowsFonts("Arial" = windowsFont("Arial"))

full_plot2 <- df_final2 %>% 
  ggplot(aes(x = status, y = persentase, fill = factor(gp))) +
  geom_bar(
    data = df_final2,
    stat = "identity"
  ) +
  geom_text(
    data = df_final2 %>%
      dplyr::filter(
        group == 1
      ),
    aes(y = persentase - 3, label = paste0(persentase, "%")),
    color = "white", fontface = 2
  ) +
  scale_x_discrete(labels = c('Bukan Penerima \n BPUM', 'Penerima \n BPUM')) +
  facet_grid( ~ nama) +
  scale_fill_manual('Status', labels = c('ada', 'tidak'), values = my_colors) +
  labs(
    title = "Karakteristik Aset",
    y = "Persentase",
    x = "Variabel"
  ) +
  # guides(fill=guide_legend(ncol=2)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title.position = "plot",
    # top,right,bottom,left
    plot.title = element_text(size = rel(1.75), face = "bold", margin = margin(0, 0, 10, 10)),
    axis.title.x = element_text(colour = "gray50", size = 13),
    axis.title.y = element_text(colour = "gray50", size = 13),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    # text label facet grid
    strip.text.x = element_text(size = 12, colour = 'slategray', face = 'bold'),
    legend.position = 'none'
  )

full_plot2


plot_grid(
  full_plot2,
  plot_grid(
    NULL, 
    get_legend(bkn_penerima2),
    get_legend(penerima2),
    NULL,
    nrow = 4, align = "v"
  ),
  nrow = 1,
  rel_widths = c(9, 2)
)

ggsave2(
  filename = "E:/Visualisasi/aset.png",
  width = 10,
  height = 5
)



# 4 -----------------------------------------------------------------------
s4 <- 'Pendidikan KRT (1:>= SMA, 0: lainnya)	8,7	8,16'

df4 <- dm::read_pattern(
  s4,
  pos_non_angka = 1,
  pos_angka = 2:3
) %>%
  extract(
    v1,
    into = c("nama", "status"),
    regex = "(\\X*)\\s\\((\\X*)\\)"
  ) %>%
  mutate_at(
    3:4, ~ str_replace_all(.x, ",", ".")
  ) %>%
  type_convert() %>%
  rename(
    bkn_penerima = v2,
    penerima = v3
  ) %>%
  mutate(
    urutan = 1:n(),
    group = 1
  ) %>% 
  dplyr::select(-status)

df4

df_final4 <- df4 %>%
  add_row(
    df4 %>%
      mutate_at(2:3, ~ 100 - .x) %>%
      mutate(group = 0)
  ) %>%
  pivot_longer(2:3, names_to = "status", values_to = "persentase") %>%
  mutate(
    gp = case_when(
      group == 0 & status == "penerima" ~ 1,
      group == 1 & status == "penerima" ~ 2,
      group == 0 & status == "bkn_penerima" ~ 3,
      group == 1 & status == "bkn_penerima" ~ 4
    )
  )

df_final4

my_labels <- paste0(c(seq(100, 0, -50), seq(50, 100, 50)), "%")
my_labels
my_colors <- c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")



bkn_penerima4 <- df_final4 %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  geom_bar(data = subset(df_final4, status == "bkn_penerima"), stat = "identity") +
  scale_fill_manual("Bukan Penerima BPUM", labels = c('>= SMA', 'Lainnya'), values = my_colors[3:4])
bkn_penerima4


penerima4 <- df_final4 %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  geom_bar(data = subset(df_final4, status == "penerima"), stat = "identity") +
  scale_fill_manual("Penerima BPUM", labels = c('>= SMA', 'Lainnya'), values = my_colors[1:2])
penerima4


df_final4 <- df_final4 %>% 
  dplyr::filter(nama != 'Jumlah ART')


df_final4
windowsFonts("Arial" = windowsFont("Arial"))

full_plot4 <- df_final4 %>% 
  ggplot(aes(x = status, y = persentase, fill = factor(gp))) +
  geom_bar(
    data = df_final4,
    stat = "identity"
  ) +
  geom_text(
    data = df_final4 %>%
      dplyr::filter(
        group == 1
      ),
    aes(y = persentase - 3, label = paste0(persentase, "%")),
    color = "white", fontface = 2
  ) +
  scale_x_discrete(labels = c('Bukan Penerima \n BPUM', 'Penerima \n BPUM')) +
  facet_grid( ~ nama) +
  scale_fill_manual('Status', labels = c('ada', 'tidak'), values = my_colors) +
  labs(
    title = "Karakteristik Sosial",
    y = "Persentase",
    x = "Variabel"
  ) +
  # guides(fill=guide_legend(ncol=2)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title.position = "plot",
    # top,right,bottom,left
    plot.title = element_text(size = rel(1.75), face = "bold", margin = margin(0, 0, 10, 10)),
    axis.title.x = element_text(colour = "gray50", size = 13),
    axis.title.y = element_text(colour = "gray50", size = 13),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    # text label facet grid
    strip.text.x = element_text(size = 12, colour = 'slategray', face = 'bold'),
    legend.position = 'none'
  )

full_plot4





# Sosial ------------------------------------------------------------------
plot_grid(
  full_plot4,
  plot_grid(
    NULL, 
    get_legend(bkn_penerima4),
    get_legend(penerima4),
    NULL,
    nrow = 4, align = "v"
  ),
  nrow = 1,
  rel_widths = c(9, 2)
)

ggsave2(
  filename = "E:/Visualisasi/sosial.png",
  width = 10,
  height = 5
)

# deografi ----------------------------------------------------------------
library(gridExtra)
tb <- tableGrob(
  tibble(
    'Penerima BPUM' =	4.15,
    'Bukan Penerima BPUM' = 3.96
  )
)

plot_grid(
  full_plot4,
  plot_grid(
    get_legend(bkn_penerima),
    get_legend(penerima),
    tb,
    nrow = 3, align = "v"
  ),
  nrow = 1,
  rel_widths = c(9, 2)
)

ggsave2(
  filename = "E:/Visualisasi/demografi.png",
  width = 10,
  height = 5
)


