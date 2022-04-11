s <- "Status bangunan (1: milik sendiri, 0: lainnya)	85,4	81,99
Luas lantai perkapita (1: >            , 0: lainnya)	92,14	90,55
Jenis atap (1: beton/genteng/seng/asbes, 0 lainnya)	97,74	98,67
Jenis dinding (1: tembok, 0: lainnya)	67,92	74,26
Jenis lantai (1: marmer/granit/keramik/parket/vinil/karpet/ubin/tegel/teraso/semen/batu bata, 0 lainnya)	79,2	83,95
Fasilitas BAB (1: milik sendiri, 0: lainnya)	84,57	87,13
Sumber air minum (1:air kemasan bermerk/air isi ulang/leding/sumur bor/pompa/sumur terlindungi/mata air terlindungi, 0: lainnya)	85,22	90,72
Sumber penerangan (1: listrik, 0: lainnya)	98,93	99,84
Jenis bahan bakar memasak (1: tidak memasak di rumah/listrik/gas elpiji/biogas/gas kota/briket, 0: lainnya)	76,66	84,75
"

df <-
  dm::read_pattern(
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
    urutan = 1:9,
    group = 1
  ) %>%
  dplyr::select(-status)


df_final <- df %>%
  add_row(
    df %>%
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

df_final

my_labels <- paste0(c(seq(100, 0, -50), seq(50, 100, 50)), "%")
my_labels
my_colors <- c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")

full_plot <-
  df_final %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  # geom_vline(xintercept = 0) +
  geom_bar(data = subset(df_final, status == "bkn_penerima"), aes(x = -persentase), stat = "identity", width = 0.9) +
  geom_bar(data = subset(df_final, status == "penerima"), stat = "identity", width = 0.9) +
  geom_text(
    data = df_final %>%
      dplyr::filter(
        status == "bkn_penerima",
        group == 1
      ),
    aes(x = -persentase + 15, label = paste0(persentase, " %")), color = "white", fontface = 2
  ) +
  geom_text(
    data = df_final %>%
      dplyr::filter(
        status == "penerima",
        group == 1
      ),
    aes(x = persentase - 15, label = paste0(persentase, " %")), color = "white", fontface = 2
  ) +
  scale_x_continuous(labels = my_labels, expand = expansion(mult = c(0.005, 0))) +
  # scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(NULL, labels = c(0, 1, 0, 1), values = my_colors) +
  labs(
    title = "Karakteristik Tempat Tinggal",
    x = "Persentase",
    y = "Variabel"
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
    panel.grid = element_blank(),
    legend.position = "none"
  )
full_plot

bkn_penerima <- df_final %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  geom_bar(data = subset(df_final, status == "bkn_penerima"), stat = "identity") +
  scale_fill_manual("Bukan Penerima BPUM", labels = c(0, 1), values = my_colors[3:4])
bkn_penerima


penerima <- df_final %>%
  ggplot(aes(x = persentase, y = reorder(nama, -urutan), fill = factor(gp), group = group)) +
  geom_bar(data = subset(df_final, status == "penerima"), stat = "identity") +
  scale_fill_manual("Penerima BPUM", labels = c(0, 1), values = my_colors[1:2])

library(cowplot)
plot_grid(
  full_plot,
  plot_grid(
    get_legend(bkn_penerima),
    get_legend(penerima),
    NULL,
    nrow = 3, align = "v"
  ),
  nrow = 1,
  rel_widths = c(8, 2)
)

ggsave2(
  filename = "E:/Visualisasi/tempattinggal.png",
  width = 10,
  height = 5
)
