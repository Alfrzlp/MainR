library(tidyverse)
library(dm)

my_colors <- c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")

# data --------------------------------------------------------------------

s <- 'Pendidikan KRT (1: >= SMA, 0: lainnya)	2,71	1,74	3,03	4,35	4,31	4,49	4,14	6,75	5,73	4,68	6,96	6,99	8,01	9,89	10,82	9,88	17,65	16,26	26,73	20
Usia KRT (1: antara 15 sampai 65, 0: lainnya)	84,3	88,81	86,84	89,03	87,35	91,21	88,69	90,99	89,26	91,64	88,96	90,99	89,73	92,99	90,43	89,53	91,01	90,33	90,89	91,34
Jenis Kelamin KRT SMA Sederajat ke Atas (1: laki-laki, 0: perempuan)	87,97	91,54	89,77	90,68	88,87	89,84	87,51	90,24	87,44	86,45	87,54	89,34	87,21	89,03	86,16	84,88	84,93	87,45	86,26	80,9
Jumlah ART (dalam orang)	4,98	5,13	4,47	4,85	4,28	4,72	4,16	4,45	3,99	4,27	3,84	4,02	3,71	3,83	3,54	3,62	3,32	3,42	3,13	3
Status Pernikahan KRT (1: kawin, 0: lainnya)	86,81	89,8	87,58	89,86	87,02	89,06	85,22	87,62	84,95	85,28	84,17	88,79	83,56	88,67	82,11	83,33	80,1	84,57	79,25	75,52
Sektor Pekerjaan KRT (1: nonpertanian, 0: lainnya)	64,66	71,64	70,25	75,98	74,95	84,96	77,25	80,68	77,32	86,79	80,26	85,11	82,11	86,51	84,55	84,5	87,77	89,09	88,87	91,64
Kepemilikan Tabungan (1: ada, 0: tidak)	30,28	48,01	35,85	49,48	40,92	58,4	42,68	59,85	45,56	62,54	51,54	65,63	57,49	65,83	62,8	70,74	72,25	77,98	83,6	84,48
Kepemilikan Kredit (1: ada, 0: tidak)	22,03	35,82	23,82	37,47	25,73	33,01	26,09	33,77	30,07	37,79	31,52	35,66	32,24	38,49	32,97	37,6	34,27	41,98	34,38	39,4
Akses Internet (1: akses, 0: tidak)	23,64	36,07	31,79	39,75	35,56	47,66	38,27	48,59	42,63	50,5	47,11	54,78	51,92	56,83	56,95	58,91	66,58	70,16	78,5	80,9
Status Wilayah (1: perkotaan, 0: perdesaan)	27,7	46,52	32,82	56,94	38,56	55,08	38,94	57,6	39,32	55,85	40,89	52,76	43,22	48,92	46,31	54,65	57,44	58,23	68,95	72,24
Kepulauan tempat tinggal (1: pulau jawa, 0: lainnya)	34,34	42,29	35,29	41,82	32,44	38,28	29,08	31,33	27,82	30,77	29,5	33,64	26,53	32,91	27,71	38,37	31,5	36,01	35,49	48,36
Kulkas (1: ada, 0: tidak)	33,92	47,51	47,21	62,11	53,03	66,21	58,16	69,23	64,03	76,25	68,19	78,86	73,96	80,04	78,06	83,91	84,38	86,83	86,5	90,75
AC (1: ada, 0: tidak)	0,06	0,75	0,64	0,21	0,87	0,98	1,55	1,31	2,35	2,17	3,55	4,41	5,38	4,5	8,75	7,95	15,81	12,76	34,06	23,88
PC/komputer (1: ada, 0: tidak)	3,32	9,45	6,49	8,49	8,88	12,5	10,8	15,95	14,15	17,39	16,97	19,85	21,32	21,76	25,81	25,58	35,78	37,04	50,5	47,76
Motor (1: ada, 0: tidak)	64,43	73,38	73,82	84,27	78,29	85,74	79,17	82,55	81,67	87,96	84,06	88,05	86,33	87,95	87,42	87,21	88,17	87,65	88,28	86,87
Mobil (1: ada, 0: tidak)	1,87	1,49	2,86	2,69	3,77	5,66	6,9	4,69	8,94	8,86	12,21	7,54	16,29	13,13	22,04	18,6	32,09	28,19	53,9	40
Status bangunan (1: milik sendiri, 0: lainnya)	89,71	85,32	88,6	84,89	86,15	83,01	85,06	78,42	84,36	81,1	84,31	80,51	85,09	83,09	85,05	80,43	82,65	81,69	82,14	82,99
Luas lantai perkapita (1: Luas > 8 m^2, 0: lainnya)	79,94	76,87	86,84	85,09	90	86,13	91,22	89,68	92,68	89,97	94,85	93,2	95,36	93,17	96,42	95,35	97,72	97,12	98,53	98,51
Jenis Atap (1: beton/genteng/seng/asbes, 0: lainnya)	94,49	98,01	96,93	98,14	97,03	98,83	97,37	98,5	98,45	98,66	98,19	98,35	98,37	99,1	99,1	99,03	99,01	98,97	98,93	99,1
Jenis Dinding (1: tembok, 0: lainnya)	50,56	65,92	59,56	72,05	62,63	69,92	62,71	70,17	64,72	70,74	67,94	71,51	71,63	75,36	75,45	77,33	81,02	84,36	87,64	90,15
Jenis Lantai (1: marmer/granit/keramik/parket/vinil/karpet/ubin/tegel/teraso/semen/batu bata, 0: lainnya)	64,24	77,86	72,3	83,64	76,11	81,84	76,37	82,18	77,56	81,61	80,44	83,46	82,32	84,35	85,56	86,05	88,14	89,09	92,4	91,34
Fasilitas BAB (1: milik sendiri, 0: lainnya)	70,65	76,12	78,12	80,54	79,05	84,77	83,34	88,18	86,12	87,79	87,04	87,32	88,31	89,93	89,75	90,89	92,63	91,98	93,31	92,84
Sumber Air Minum (1: air kemasan bermerk/air isi ulang/leding/sumur bor/pompa/sumur terlindungi/mata air terlindungi, 0: lainnya)	76,01	82,59	79,25	90,06	82,82	89,26	83,1	88,37	83,81	88,63	85,87	91,91	88,03	92,63	89,25	93,02	91,78	94,44	94,57	97,01
Sumber Penerangan (1: listrik, 0: lainnya)	96,36	100	97,92	99,79	98,77	99,8	99,23	99,81	99,34	100	99,4	99,82	99,43	99,46	99,75	99,81	99,71	100	99,76	100
Jenis Bahan Bakar Memasak (1: tidak memasak di rumah/listrik/gas elpiji/biogas/gas kota/briket, 0: lainnya)	54,14	67,91	67,01	80,54	73,47	83,4	74,69	85,93	77,84	85,62	80,87	84,01	83,42	86,51	84,23	88,95	87,21	90,95	87,6	92,54
'

desil_levels <- str_glue('desil{rep(1:10)}')
kat <- c('Karakteristik Sosial', 'Karakteristik Demografi', 'Karakteristik Ekonomi', 'Karakteristik Wilayah', 'Karakteristik Tempat Tinggal', 'Karakteristik Asset')
n_kat <- c(1, 4, 4, 2, 5, 9)


dat <- 
  dm::read_pattern(
    s, pos_non_angka = 1,
    pos_angka = 2:21
  ) %>% 
  extract(
    v1,
    into = c("nama", "status"),
    regex = "(\\X*)\\s\\((\\X*)\\)"
  ) %>% 
  mutate(
    across(v2:v21, ~str_replace_all(., ',', '\\.'))
  ) %>% 
  mutate(
    kat = str_remove_all(rep(kat, n_kat), 'Karakteristik '),
    .before = nama
  ) %>% 
  type_convert() %>% 
  setNames(
    c('kat', 'nama', 'status', str_glue('desil{rep(1:10, each = 2)}_{rep(0:1, 10)}'))
  ) %>% 
  dplyr::filter(nama != 'Jumlah ART') 


head(dat)

my_legend_text <- str_split(dat$status, ', ') %>% 
  lapply(function(x) str_remove(x, '[0-1]:\\s'))



# legend ------------------------------------------------------------------
bkn_penerima <- 
  df_final %>%
  ggplot(
    aes(x = persentase, y = factor(sk, levels = desil_levels), fill = factor(gp))
  ) +
  geom_bar(data = subset(df_final, bpum == 0), stat = "identity") +
  scale_fill_manual(
    "Bukan Penerima BPUM",
    labels = str_wrap(my_legend_text[[1]], width = 20),
    values = rev(my_colors[3:4]),
    guide = guide_legend(
      direction = "vertical",
      title.position = "top"
    )
  ) 

bkn_penerima


suppressMessages({
  penerima <- bkn_penerima +
    scale_fill_manual(
      "Penerima BPUM",
      values = rev(my_colors[1:2]),
      labels = str_wrap(my_legend_text[[1]], width = 20),
      guide = guide_legend(
        direction = "vertical",
        title.position = "top"
      )
    )
})

penerima
# viz ---------------------------------------------------------------------
my_labels <- paste0(c(seq(100, 0, -50), seq(50, 100, 50)), "%")
i <- 3

df_final <- dat[i, ] %>% 
  mutate(group = 1) %>% 
  add_row(
    dat[i, ] %>%
      mutate_at(-c(1:3), ~ 100 - .x) %>% 
      mutate(group = 0)
  ) %>% 
  pivot_longer(
    contains('desil'),
    values_to = 'persentase',
    names_to = 'sk'
  ) %>% 
  mutate(
    bpum = substr(sk, nchar(sk), nchar(sk)),
    sk = str_remove_all(sk, "_(0|1)$"),
    sk = str_replace_all(sk, "_", " "),
  ) %>% 
  type_convert() %>% 
  mutate(
    gp = case_when(
      group == 0 & bpum == 1 ~ 1,
      group == 1 & bpum == 1 ~ 2,
      group == 0 & bpum == 0 ~ 3,
      group == 1 & bpum == 0 ~ 4
    )
  ) 

glimpse(df_final)




ggplot(
    data = df_final,
    aes(x = persentase, y = factor(sk, levels = desil_levels), fill = factor(group))
  ) +
  geom_bar(data = subset(df_final, bpum == 0), aes(x = -persentase), stat = "identity", width = 0.9) +
  geom_bar(data = subset(df_final, bpum == 1), stat = "identity", width = 0.9) +
  geom_text(
    data = ~ .x %>%
      dplyr::filter(
        bpum == 0,
        group == 1
      ),
    aes(x = -persentase, label = paste0(persentase, "%")),
    color = "white", fontface = 2,
    nudge_x = 10
  ) +
  geom_text(
    data = ~ .x %>%
      dplyr::filter(
        bpum == 1,
        group == 1
      ),
    aes(x = persentase, label = paste0(persentase, "%")),
    color = "white", fontface = 2,
    nudge_x = -10
  ) +
  scale_y_discrete(
    labels = c(as.vector(str_glue('Desil {rep(1:9)} ')), 'Desil 10')
  ) +
  scale_x_continuous(
    # labels = my_labels, 
    # breaks = function(x) {
    #   if(sum(x > -1) > 0){
    #     seq(0, 100, by = 25)
    #   } else if(sum(x > -1) <= 0) {
    #     seq(-100, 0, by = 25)
    #   }
    # },
    labels = function(x) unique(scales::percent(abs(x)/100)),
    expand = expansion(mult = c(0.005, 0))
  ) +
  scale_fill_manual(
    NULL, 
    labels = function(x) str_wrap(x, 15),
    values = c("#E4CFA1", "#49A59B")) 
  labs(
    title = unique(df_final$nama),
    x = "Persentase",
    y = "Desil"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title.position = "plot", 
    panel.spacing = unit(0, 'mm'),
    plot.title = element_text(size = rel(1.75), face = "bold", margin = margin(0, 0, 10, 10)),
    axis.title.x = element_text(colour = "black", size = 13.5, margin = margin(15, 0, 0, 0)),
    axis.title.y = element_text(colour = "black", size = 13.5, margin = margin(0, 15, 0, 0)),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank()
  ) + 
  facet_grid(
    ~ bpum, scales = 'free_x',
    labeller = my_labeller
  )


full_plot
my_labeller <- as_labeller(c('0' = 'Bukan Penerima BPUM', '1' = 'Penerima BPUM'))





ggsave(
  filename = str_glue("E:/Visualisasi/rev/jk.png"),
  width = 10,
  height = 5,
  units = "in", 
  dpi = 300,
  scale = 0.85, 
  bg = 'white'
)








legend_plot <- plot_grid(
  NULL,
  get_legend(bkn_penerima),
  get_legend(penerima),
  NULL,
  vjust = 0, rel_heights = c(0.5, 1.5, 1.5, 0.5),
  nrow = 4, align = "v"
)

plot_grid(
  full_plot,
  legend_plot,
  nrow = 1,
  rel_widths = c(10, 3)
)


# full data final ---------------------------------------------------------

full_data_final <- dat %>% 
  split(1:nrow(.)) %>% 
  lapply(
    function(x){
      x %>% 
        mutate(group = 1) %>% 
        add_row(
          x %>%
            mutate_at(-c(1:3), ~ 100 - .x) %>% 
            mutate(group = 0)
        ) %>% 
        pivot_longer(
          contains('desil'),
          values_to = 'persentase', names_to = 'sk'
        ) %>% 
        mutate(
          bpum = substr(sk, nchar(sk), nchar(sk)),
          sk = str_remove_all(sk, "_(0|1)$"),
          sk = str_replace_all(sk, "_", " "),
        ) %>% 
        type_convert() %>% 
        mutate(
          gp = case_when(
            group == 0 & bpum == 1 ~ 1,
            group == 1 & bpum == 1 ~ 2,
            group == 0 & bpum == 0 ~ 3,
            group == 1 & bpum == 0 ~ 4
          )
        ) 
    }
  )

full_data_final


# Walk --------------------------------------------------------------------
my_names <- dat$nama

full_data_final %>% 
  walk2(
    .x = .,
    .y = as.numeric(names(.)),
    ~ {
      message("Generating plot for ", my_names[.y])
      
      full_plot <- ggplot(
        data = .x,
        aes(x = persentase, y = factor(sk, levels = desil_levels), fill = factor(gp))
      ) +
        geom_bar(data = subset(.x, bpum == 0), aes(x = -persentase), stat = "identity", width = 0.9) +
        geom_bar(data = subset(.x, bpum == 1), stat = "identity", width = 0.9) +
        geom_text(
          data = .x %>%
            dplyr::filter(
              bpum == 0,
              group == 1
            ),
          aes(x = -persentase, label = paste0(persentase, "%")),
          color = "white", fontface = 2,
          nudge_x = 15
        ) +
        geom_text(
          data = .x %>%
            dplyr::filter(
              bpum == 1,
              group == 1
            ),
          aes(x = persentase, label = paste0(persentase, "%")),
          color = "white", fontface = 2,
          nudge_x = -15
        ) +
        scale_y_discrete(
          labels = str_glue('Desil {rep(1:10)}')
        ) +
        scale_x_continuous(labels = my_labels, expand = expansion(mult = c(0.005, 0))) +
        scale_fill_manual(
          NULL, labels = c(0, 1, 0, 1),
          values = c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")) +
        labs(
          title = my_names[.y],
          x = "Persentase",
          y = "Desil"
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
      
      
      bkn_penerima <- 
        df_final %>%
        ggplot(
          aes(x = persentase, y = factor(sk, levels = desil_levels), fill = factor(gp))
        ) +
        geom_bar(data = subset(df_final, bpum == 0), stat = "identity") +
        scale_fill_manual(
          "Bukan Penerima BPUM",
          labels = str_wrap(my_legend_text[[.y]], width = 20),
          values = rev(my_colors[3:4]),
          guide = guide_legend(
            direction = "vertical",
            title.position = "top"
          )
        ) 
      suppressMessages({
        penerima <- bkn_penerima +
          scale_fill_manual(
            "Penerima BPUM",
            labels = str_wrap(my_legend_text[[.y]], width = 20),
            values = rev(my_colors[1:2]),
            guide = guide_legend(
              direction = "vertical",
              title.position = "top"
            )
          )
      })
      
      
      legend_plot <- plot_grid(
        NULL,
        get_legend(bkn_penerima),
        get_legend(penerima),
        NULL,
        vjust = 0, rel_heights = c(0.5, 1.5, 1.5, 0.5),
        nrow = 4, align = "v"
      )
      
      p <- plot_grid(
        full_plot,
        legend_plot,
        nrow = 1,
        rel_widths = c(10, 3)
      )
      
      ggsave(
        plot = p,
        filename = str_glue("E:/Visualisasi/desil2/{str_replace_all(my_names[.y], '/', '_')}.png"),
        width = 10,
        height = 5,
        units = "in", 
        dpi = 300,
        scale = 0.85,
        bg = 'white'
      )
    }
  )


# Khusus ------------------------------------------------------------------
# AC
# Mobil

# Pendidikan KRT
# Komputer


i <- which(my_names %in% c('Pendidikan KRT', 'AC', 'PC/komputer', 'Mobil'))
my_names[i]

full_data_final[i] %>% 
  walk2(
    .x = .,
    .y = as.numeric(names(.)),
    ~ {
      message("Generating plot for ", my_names[.y])
      
      full_plot <- ggplot(
        data = .x,
        aes(x = persentase, y = factor(sk, levels = desil_levels), fill = factor(gp))
      ) +
        geom_bar(data = subset(.x, bpum == 0), aes(x = -persentase), stat = "identity", width = 0.9) +
        geom_bar(data = subset(.x, bpum == 1), stat = "identity", width = 0.9) +
        geom_text(
          data = .x %>%
            dplyr::filter(
              bpum == 0,
              group == 1
            ),
          aes(x = -persentase, label = paste0(persentase, "%")),
          color = "white", fontface = 2,
          nudge_x = -10
        ) +
        geom_text(
          data = .x %>%
            dplyr::filter(
              bpum == 1,
              group == 1
            ),
          aes(x = persentase, label = paste0(persentase, "%")),
          color = "white", fontface = 2,
          nudge_x = 10
        ) +
        scale_y_discrete(
          labels = str_glue('Desil {rep(1:10)}')
        ) +
        scale_x_continuous(labels = my_labels, expand = expansion(mult = c(0.005, 0))) +
        scale_fill_manual(
          NULL, labels = c(0, 1, 0, 1),
          values = c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")) +
        labs(
          title = my_names[.y],
          x = "Persentase",
          y = "Desil"
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
      
      
      bkn_penerima <- 
        df_final %>%
        ggplot(
          aes(x = persentase, y = factor(sk, levels = desil_levels), fill = factor(gp))
        ) +
        geom_bar(data = subset(df_final, bpum == 0), stat = "identity") +
        scale_fill_manual(
          "Bukan Penerima BPUM",
          labels = str_wrap(my_legend_text[[.y]], width = 20),
          values = rev(my_colors[3:4]),
          guide = guide_legend(
            direction = "vertical",
            title.position = "top"
          )
        ) 
      
      suppressMessages({
        penerima <- bkn_penerima +
          scale_fill_manual(
            "Penerima BPUM",
            labels = str_wrap(my_legend_text[[.y]], width = 20),
            values = rev(my_colors[1:2]),
            guide = guide_legend(
              direction = "vertical",
              title.position = "top"
            )
          )
      })
      
      
      legend_plot <- plot_grid(
        NULL,
        get_legend(bkn_penerima),
        get_legend(penerima),
        NULL,
        vjust = 0, rel_heights = c(0.5, 1.5, 1.5, 0.5),
        nrow = 4, align = "v"
      )
      
      p <- plot_grid(
        full_plot,
        legend_plot,
        nrow = 1,
        rel_widths = c(10, 3)
      )
      
      ggsave(
        plot = p,
        filename = str_glue("E:/Visualisasi/desil2/{str_replace_all(my_names[.y], '/', '_')}.png"),
        width = 10,
        height = 5,
        units = "in", dpi = 300,
        scale = 0.85, bg = 'white'
      )
    }
  )


