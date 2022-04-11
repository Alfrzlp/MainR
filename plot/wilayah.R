s3 <- "Status wilayah (1: perkotaan, 0: perdesaan)	42,91	55,41
Kepulauan tempat tinggal (1: pulau jawa, 0: lainnya)	30,94	36,66"

df3 <- dm::read_pattern(
  s3,
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

df3

df_final3 <- df3 %>%
  add_row(
    df3 %>%
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

df_final3

my_colors <- c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")



bkn_penerima3 <- df_final3 %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  geom_bar(data = subset(df_final3, status == "bkn_penerima"), stat = "identity") +
  scale_fill_manual("Bukan Penerima BPUM",
                    labels = c('Kepulauan Tempat Tinggal : Pulau Jawa
                               Status Wilayah : Perkotaan',
                               'Kepulauan Tempat Tinggal : Lainnya
                               Status Wilayah :Perdesaan'), values = my_colors[3:4])
bkn_penerima3


penerima3 <- df_final3 %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  geom_bar(data = subset(df_final3, status == "penerima"), stat = "identity") +
  scale_fill_manual("Penerima BPUM",
                    labels = c('Kepulauan Tempat Tinggal : Pulau Jawa
                               Status Wilayah : Perkotaan',
                               'Kepulauan Tempat Tinggal : Lainnya
                               Status Wilayah :Perdesaan'), values = my_colors[1:2])
penerima3




df_final3
windowsFonts("Arial" = windowsFont("Arial"))

full_plot3 <- df_final3 %>% 
  ggplot(aes(x = status, y = persentase, fill = factor(gp))) +
  geom_bar(
    data = df_final3,
    stat = "identity"
  ) +
  geom_text(
    data = df_final3 %>%
      dplyr::filter(
        group == 1
      ),
    aes(y = persentase - 5, label = paste0(persentase, "%")),
    color = "white", fontface = 2
  ) +
  scale_x_discrete(labels = c('Bukan Penerima \n BPUM', 'Penerima \n BPUM')) +
  facet_grid( ~ nama) +
  scale_fill_manual('Status', labels = c('ada', 'tidak'), values = my_colors) +
  labs(
    title = "Karakteristik Wilayah",
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

full_plot3


plot_grid(
  full_plot3,
  plot_grid(
    NULL, 
    get_legend(bkn_penerima),
    get_legend(penerima),
    NULL,
    nrow = 4, align = "v"
  ),
  nrow = 1,
  rel_widths = c(9, 2)
)

ggsave2(
  filename = "E:/Visualisasi/wilayah.png",
  width = 10,
  height = 5
)







p1 <- df_final3 %>% 
  dplyr::filter(nama == 'Kepulauan tempat tinggal') %>% 
  ggplot(aes(x = status, y = persentase, fill = factor(gp))) +
  geom_bar(stat = 'identity') +
  geom_text(
    data = df_final3 %>%
      dplyr::filter(
        nama == 'Kepulauan tempat tinggal',
        group == 1
      ),
    aes(y = persentase + 5, label = paste0(persentase, "%")),
    color = "slategray", fontface = 2
  ) +
  scale_x_discrete(labels = c('Bukan Penerima \n BPUM', 'Penerima \n BPUM')) +
  facet_grid( ~ nama) +
  scale_fill_manual('Status', labels = c('ada', 'tidak'), values = my_colors) +
  labs(
    title = "Karakteristik Wilayah",
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
    strip.text.x = element_text(size = 12, colour = 'slategray', face = 'bold'),
    legend.position = 'none'
  )


plot_grid(
  p1,
  plot_grid(
    NULL, 
    get_legend(bkn_penerima),
    get_legend(penerima),
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